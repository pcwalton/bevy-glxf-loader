//! A simple loader for glXF files.

use crate::glxf::{Asset as GlxfAsset, AssetHeader, AssetTransform, Glxf as GlxfGlxf, Node};
use bevy::{
    app::{Plugin, SpawnScene},
    asset::{
        io::Reader, Asset, AssetApp, AssetLoader, AssetPath, AssetServer, Handle, LoadContext,
        LoadDirectError, ParseAssetPathError, UntypedHandle,
    },
    ecs::{
        query::Without,
        system::{Commands, Query, Res},
    },
    log::{error, warn},
    math::{Mat4, Quat, Vec3},
    platform::collections::hash_map::HashMap,
    prelude::{
        App, AppTypeRegistry, Component, Deref, DerefMut, Entity, EntityWorldMut, FromWorld, Name,
        ReflectComponent, ReflectDefault, Transform, Visibility, World,
    },
    reflect::{
        serde::{ReflectDeserializerProcessor, TypedReflectDeserializer},
        DynamicEnum, DynamicTuple, DynamicVariant, GenericInfo, PartialReflect, Reflect,
        ReflectDeserialize, TypeRegistration, TypeRegistry,
    },
    scene::{Scene, SceneRoot},
    utils::default,
};
use serde::{
    de::{DeserializeSeed as _, Error as DeserializeError, Visitor},
    Deserializer,
};
use serde_json::Error as JsonError;
use std::{
    fmt::{Formatter, Result as FmtResult},
    io::Error as IoError,
    path::{Path, PathBuf},
};
use thiserror::Error;

pub mod glxf;

pub struct GlxfPlugin;

#[derive(Clone)]
pub struct GlxfLoader {
    type_registry: AppTypeRegistry,
}

#[derive(Component, Reflect, Deref, DerefMut)]
pub struct GlxfScene(pub Handle<Glxf>);

#[derive(Error, Debug)]
pub enum GlxfLoadError {
    #[error("An I/O error occurred: {0}")]
    Io(#[from] IoError),
    #[error("Parse failed: {0}")]
    Parse(#[from] JsonError),
    #[error("Asset loading failed: {0}")]
    AssetLoad(#[from] Box<LoadDirectError>),
    #[error("No node was present: {0}")]
    NoNodePresent(u32),
}

struct GlxfSpawner<'a> {
    world: World,
    type_registry: AppTypeRegistry,
    node_index_to_entity: HashMap<u32, Entity>,
    scene_index: u32,
    glxf: &'a GlxfGlxf,
    root_asset_path: AssetPath<'static>,
}

struct GlxfReflectDeserializer<'a, 'lc> {
    load_context: &'a mut LoadContext<'lc>,
}

#[derive(Asset, Reflect, Clone, Default)]
pub struct Glxf {
    pub glxf: GlxfGlxf,
    #[reflect(ignore)]
    pub named_scenes: HashMap<Box<str>, Handle<Scene>>,
    pub scenes: Vec<Handle<Scene>>,
    pub default_scene: Option<Handle<Scene>>,
}

#[derive(Clone, Component, Reflect, Deref, DerefMut)]
#[reflect(Component)]
pub struct GlxfNodeIndex(pub usize);

#[derive(Clone, Component, Reflect)]
#[reflect(Component)]
pub struct GlxfSceneRoot {
    pub root: AssetPath<'static>,
    pub asset: GlxfAsset,
}

impl Plugin for GlxfPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<AssetHeader>()
            .register_type::<AssetTransform>()
            .register_type::<Glxf>()
            .register_type::<GlxfAsset>()
            .register_type::<GlxfGlxf>()
            .register_type::<GlxfNodeIndex>()
            .register_type::<GlxfScene>()
            .register_type::<GlxfSceneRoot>()
            .init_asset::<Glxf>()
            .init_asset_loader::<GlxfLoader>()
            .add_systems(SpawnScene, spawn_glxf_scenes);
    }
}

impl AssetLoader for GlxfLoader {
    type Asset = Glxf;

    type Settings = ();

    type Error = GlxfLoadError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _: &Self::Settings,
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut buffer = vec![];
        reader.read_to_end(&mut buffer).await?;
        let glxf: GlxfGlxf = serde_json::from_slice(&buffer)?;

        let mut named_scenes = HashMap::default();
        let mut scenes = vec![];

        let asset_path = load_context.asset_path();
        match asset_path.parent() {
            Some(root_asset_path) => {
                for (glxf_scene_index, glxf_scene) in glxf.scenes.iter().enumerate() {
                    let scene = GlxfSpawner::new(
                        &glxf,
                        root_asset_path.clone(),
                        self.type_registry.clone(),
                        glxf_scene_index as u32,
                    )
                    .spawn(load_context)?;

                    let scene_handle =
                        load_context.add_labeled_asset(format!("Scene{}", glxf_scene_index), scene);

                    if let Some(name) = &glxf_scene.name {
                        named_scenes.insert(name.clone().into_boxed_str(), scene_handle.clone());
                    }

                    scenes.push(scene_handle);
                }
            }
            None => {
                error!(
                    "glXF at `{}` has no parent asset path. URIs of assets will not load.",
                    asset_path
                );
            }
        }

        Ok(Glxf {
            glxf,
            // FIXME(pcwalton): This is wrong
            default_scene: scenes.first().cloned(),
            scenes,
            named_scenes,
        })
    }

    fn extensions(&self) -> &[&str] {
        &["glxf"]
    }
}

impl<'a> GlxfSpawner<'a> {
    fn new(
        glxf: &'a GlxfGlxf,
        root_asset_path: AssetPath<'static>,
        type_registry: AppTypeRegistry,
        scene_index: u32,
    ) -> GlxfSpawner<'a> {
        GlxfSpawner {
            world: default(),
            type_registry,
            node_index_to_entity: default(),
            scene_index,
            glxf,
            root_asset_path,
        }
    }

    fn spawn(mut self, load_context: &mut LoadContext<'_>) -> Result<Scene, GlxfLoadError> {
        if let Some(root_node_indices) = &self.glxf.scenes[self.scene_index as usize].nodes {
            for root_node_index in root_node_indices {
                self.load_node(load_context, *root_node_index)?;
            }
        }

        Ok(Scene::new(self.world))
    }

    fn load_node(
        &mut self,
        load_context: &mut LoadContext<'_>,
        node_index: u32,
    ) -> Result<(), GlxfLoadError> {
        if self.node_index_to_entity.contains_key(&node_index) {
            // Nothing to do.
            return Ok(());
        }

        let Some(node) = self.glxf.nodes.get(node_index as usize) else {
            return Err(GlxfLoadError::NoNodePresent(node_index));
        };

        let mut new_entity = self.world.spawn(());

        // Load transform.
        // See `bevy_gltf::loader::node_transform`.

        let transform = if let Some(matrix) = node.matrix {
            // TODO(pcwalton): Check to make sure scale/rotation/translation
            // aren't also specified.
            Transform::from_matrix(Mat4::from_cols_array_2d(&matrix))
        } else {
            Transform {
                translation: match node.translation {
                    Some(translation) => translation.into(),
                    None => Vec3::ZERO,
                },
                rotation: match node.rotation {
                    Some(rotation) => Quat::from_array(rotation),
                    None => Quat::IDENTITY,
                },
                scale: match node.scale {
                    Some(scale) => Vec3::from(scale),
                    None => Vec3::ONE,
                },
            }
        };

        new_entity.insert(transform).insert(Visibility::Inherited);

        // Add the name component.
        if let Some(ref name) = node.name {
            new_entity.insert(Name::new(name.clone()));
        }

        // Add the asset, if any.
        if let Some(asset_index) = node.asset {
            match self.glxf.assets.get(asset_index as usize) {
                None => {
                    error!("Asset {} doesn't exist", asset_index);
                }
                Some(glxf_asset) => {
                    new_entity.insert(GlxfSceneRoot {
                        asset: glxf_asset.clone(),
                        root: self.root_asset_path.clone(),
                    });
                }
            };
        }

        // Add custom components, via the `BEVY_components` extension.
        add_custom_components(&mut new_entity, node, &self.type_registry, load_context);

        // Add the `GlxfNodeIndex`.
        new_entity.insert(GlxfNodeIndex(node_index as usize));

        // Add children.
        let new_entity = new_entity.id();
        if let Some(ref children) = node.children {
            for &kid in children {
                self.load_node(load_context, kid)?;
                if let Some(kid) = self.node_index_to_entity.get(&kid) {
                    self.world.entity_mut(new_entity).add_child(*kid);
                }
            }
        }

        self.node_index_to_entity.insert(node_index, new_entity);
        Ok(())
    }
}

fn add_custom_components(
    new_entity: &mut EntityWorldMut,
    node: &Node,
    type_registry: &AppTypeRegistry,
    load_context: &mut LoadContext<'_>,
) {
    let Some(components_value) = node.extensions.get("BEVY_components") else {
        return;
    };

    let Some(components_object) = components_value.as_object() else {
        error!("`BEVY_components` must be an object");
        return;
    };

    let type_registry = type_registry.read();
    for (type_path, component_value) in components_object.iter() {
        let Some(type_registration) = type_registry.get_with_type_path(type_path) else {
            warn!("`{}` is not a registered type", type_path);
            continue;
        };

        match type_registration.data::<ReflectDeserialize>() {
            None => {
                // If there's no `ReflectDeserialize`,
                // try deserializing it using
                // `TypedReflectDeserializer`.
                let mut reflect_deserializer = GlxfReflectDeserializer::new(load_context);
                let typed_reflect_deserializer = TypedReflectDeserializer::with_processor(
                    type_registration,
                    &type_registry,
                    &mut reflect_deserializer,
                );
                match typed_reflect_deserializer.deserialize(component_value) {
                    Err(error) => {
                        error!(
                            "Failed to deserialize instance of `{}` using \
                             reflection: {:?}",
                            type_path, error
                        );
                    }
                    Ok(component) => {
                        add_component_to_entity(
                            &*component,
                            new_entity,
                            type_registration,
                            &type_registry,
                            type_path,
                        );
                    }
                }
            }

            Some(reflect_deserialize) => match reflect_deserialize.deserialize(component_value) {
                Err(error) => {
                    error!(
                        "Failed to deserialize instance of `{}`: {:?}",
                        type_path, error
                    );
                }
                Ok(component) => {
                    add_component_to_entity(
                        component.as_partial_reflect(),
                        new_entity,
                        type_registration,
                        &type_registry,
                        type_path,
                    );
                }
            },
        }
    }
}

impl<'a, 'lc> GlxfReflectDeserializer<'a, 'lc> {
    fn new(load_context: &'a mut LoadContext<'lc>) -> GlxfReflectDeserializer<'a, 'lc> {
        GlxfReflectDeserializer { load_context }
    }
}

impl ReflectDeserializerProcessor for GlxfReflectDeserializer<'_, '_> {
    fn try_deserialize<'de, D>(
        &mut self,
        registration: &TypeRegistration,
        _: &TypeRegistry,
        deserializer: D,
    ) -> Result<Result<Box<dyn PartialReflect>, D>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let type_info = registration.type_info();
        let type_path = type_info.type_path_table();
        if type_path.module_path() != Some("bevy_asset::handle")
            || type_path.ident() != Some("Handle")
        {
            return Ok(Err(deserializer));
        }
        let generics = type_info.generics();
        let GenericInfo::Type(ref asset_type) = &generics[0] else {
            error!("Handle didn't have a generic type parameter, why?");
            return Ok(Err(deserializer));
        };

        let Some(reflect_default) = registration.data::<ReflectDefault>() else {
            error!("Handle didn't have a `ReflectDefault` implementation, why?");
            return Ok(Err(deserializer));
        };

        let relative_path = match deserializer.deserialize_str(&*self) {
            Ok(path) => path,
            Err(error) => {
                error!(
                    "Failed to deserialize `{}`: {:?}",
                    type_info.type_path(),
                    error
                );
                return Err(error);
            }
        };

        let stem_pos = relative_path.find('#').unwrap_or(relative_path.len());
        let stem = relative_path_to_asset_path(&relative_path[0..stem_pos], self.load_context);
        let mut path = stem.to_string_lossy().into_owned();
        path.push_str(&relative_path[stem_pos..]);

        let untyped_handle = self
            .load_context
            .loader()
            .with_dynamic_type(asset_type.type_id())
            .load(path);

        let mut typed_handle = reflect_default.default();
        match untyped_handle {
            UntypedHandle::Strong(strong_handle) => {
                let mut tuple = DynamicTuple::default();
                tuple.insert(strong_handle);
                let new_typed_handle_enum =
                    DynamicEnum::new("Strong", DynamicVariant::Tuple(tuple));
                typed_handle.apply(&new_typed_handle_enum);
            }
            UntypedHandle::Uuid { .. } => {
                panic!("`load()` should never return weak handles");
            }
        }

        Ok(Ok(typed_handle.into_partial_reflect()))
    }
}

impl<'a> Visitor<'_> for &'a GlxfReflectDeserializer<'a, '_> {
    type Value = String;

    fn expecting(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "a string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: DeserializeError,
    {
        Ok(v.to_owned())
    }
}

fn add_component_to_entity(
    component: &dyn PartialReflect,
    new_entity: &mut EntityWorldMut,
    type_registration: &TypeRegistration,
    type_registry: &TypeRegistry,
    type_path: &str,
) {
    match type_registration.data::<ReflectComponent>() {
        None => {
            warn!("`{}` has no `ReflectComponent` data", type_path);
        }
        Some(reflect_component) => {
            reflect_component.insert(new_entity, component.as_partial_reflect(), type_registry);
        }
    }
}

impl FromWorld for GlxfLoader {
    fn from_world(world: &mut World) -> Self {
        GlxfLoader {
            type_registry: world.resource::<AppTypeRegistry>().clone(),
        }
    }
}

fn relative_path_to_asset_path(asset_path: &str, load_context: &mut LoadContext) -> PathBuf {
    let mut asset_path = Path::new(asset_path).to_owned();
    if asset_path.is_relative() {
        if let Some(parent_path) = load_context.path().parent() {
            asset_path = parent_path.join(asset_path);
        }
    }
    asset_path
}

fn spawn_glxf_scenes(
    mut commands: Commands,
    q_glxf_scene_roots: Query<(Entity, &GlxfSceneRoot), Without<SceneRoot>>,
    asset_server: Res<AssetServer>,
) {
    for (entity, glxf_scene_root) in &q_glxf_scene_roots {
        if let Ok(asset_path) = glxf_scene_root.as_asset_path() {
            let asset_path_str = asset_path.to_string();
            let scene: Handle<Scene> = asset_server.load(&asset_path_str);
            commands.entity(entity).insert(SceneRoot(scene));
        }
    }
}

impl GlxfSceneRoot {
    fn as_asset_path(&self) -> Result<AssetPath<'static>, ParseAssetPathError> {
        let mut path = self.asset.uri.clone();
        path.push('#');
        match self.asset.scene {
            Some(ref scene) => path.push_str(scene),
            None => path.push_str("Scene0"),
        }

        self.root.resolve(&path)
    }
}
