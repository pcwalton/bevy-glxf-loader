//! A simple loader for glXF files.

use crate::glxf::{Asset as GlxfAsset, AssetHeader, AssetTransform, Glxf as GlxfGlxf};
use bevy::{
    app::Plugin,
    asset::{io::Reader, Asset, AssetApp, AssetLoader, Handle, LoadContext, LoadDirectError},
    core::Name,
    gltf::Gltf,
    log::{error, warn},
    math::{Mat4, Quat, Vec3},
    prelude::{
        App, AppTypeRegistry, BuildChildren, Component, Deref, DerefMut, Entity, EntityWorldMut,
        FromWorld, ReflectComponent, Transform, Visibility, World,
    },
    reflect::{
        serde::TypedReflectDeserializer, PartialReflect, Reflect, ReflectDeserialize,
        TypeRegistration, TypeRegistry,
    },
    scene::{Scene, SceneRoot},
    utils::{default, HashMap},
};
use serde::de::DeserializeSeed as _;
use serde_json::Error as JsonError;
use std::{io::Error as IoError, path::Path};
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
    assets: &'a [LoadedGlxfAsset],
}

#[derive(Asset, Reflect, Clone, Default)]
pub struct Glxf {
    pub glxf: GlxfGlxf,
    #[reflect(ignore)]
    pub named_scenes: HashMap<Box<str>, Handle<Scene>>,
    pub scenes: Vec<Handle<Scene>>,
    pub default_scene: Option<Handle<Scene>>,
}

struct LoadedGlxfAsset {
    named_scenes: HashMap<Box<str>, Handle<Scene>>,
    default_scene: Option<Handle<Scene>>,
}

impl Plugin for GlxfPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<AssetHeader>()
            .register_type::<AssetTransform>()
            .register_type::<Glxf>()
            .register_type::<GlxfAsset>()
            .register_type::<GlxfGlxf>()
            .register_type::<GlxfScene>()
            .init_asset::<Glxf>()
            .init_asset_loader::<GlxfLoader>();
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

        let mut assets = vec![];
        for (asset_index, asset) in glxf.assets.iter().enumerate() {
            let label = format!("Asset{}", asset_index);

            let mut asset_path = Path::new(&asset.uri).to_owned();
            if asset_path.is_relative() {
                if let Some(parent_path) = load_context.path().parent() {
                    asset_path = parent_path.join(asset_path);
                }
            }

            let direct_loader = load_context.loader().immediate();

            // FIXME(pcwalton): Do better!
            let lowercase_uri = asset.uri.to_ascii_lowercase();
            if lowercase_uri.ends_with("gltf") || lowercase_uri.ends_with("glb") {
                let gltf = direct_loader
                    .load::<Gltf>(asset_path)
                    .await
                    .map_err(Box::new)?;
                let gltf_ref = gltf.get();
                assets.push(LoadedGlxfAsset {
                    named_scenes: gltf_ref.named_scenes.clone(),
                    default_scene: gltf_ref.default_scene.clone(),
                });
                load_context.add_loaded_labeled_asset(label, gltf);
            } else {
                let glxf = direct_loader
                    .load::<Glxf>(asset_path)
                    .await
                    .map_err(Box::new)?;
                let glxf_ref = glxf.get();
                assets.push(LoadedGlxfAsset {
                    named_scenes: glxf_ref.named_scenes.clone(),
                    default_scene: glxf_ref.default_scene.clone(),
                });
                load_context.add_loaded_labeled_asset(label, glxf);
            };
        }

        let mut named_scenes = HashMap::new();
        let mut scenes = vec![];
        for (glxf_scene_index, glxf_scene) in glxf.scenes.iter().enumerate() {
            let scene = GlxfSpawner::new(
                &glxf,
                self.type_registry.clone(),
                &assets[..],
                glxf_scene_index as u32,
            )
            .spawn()?;

            let scene_handle =
                load_context.add_labeled_asset(format!("Scene{}", glxf_scene_index), scene);

            if let Some(name) = &glxf_scene.name {
                named_scenes.insert(name.clone().into_boxed_str(), scene_handle.clone());
            }

            scenes.push(scene_handle);
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
        type_registry: AppTypeRegistry,
        assets: &'a [LoadedGlxfAsset],
        scene_index: u32,
    ) -> GlxfSpawner<'a> {
        GlxfSpawner {
            world: default(),
            type_registry,
            node_index_to_entity: default(),
            scene_index,
            glxf,
            assets,
        }
    }

    fn spawn(mut self) -> Result<Scene, GlxfLoadError> {
        if let Some(root_node_indices) = &self.glxf.scenes[self.scene_index as usize].nodes {
            for root_node_index in root_node_indices {
                self.load_node(*root_node_index)?;
            }
        }

        Ok(Scene::new(self.world))
    }

    fn load_node(&mut self, node_index: u32) -> Result<(), GlxfLoadError> {
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
                    let loaded_asset = self.assets.get(asset_index as usize);
                    match loaded_asset {
                        None => {
                            error!("Asset {} wasn't loaded", asset_index);
                        }
                        Some(loaded_asset) => match glxf_asset.scene {
                            Some(ref scene_name) => {
                                match loaded_asset.named_scenes.get(&**scene_name) {
                                    None => {
                                        error!(
                                            "glTF or glXF `{}` doesn't contain a scene named `{}`",
                                            glxf_asset.uri, scene_name
                                        );
                                    }
                                    Some(scene) => {
                                        new_entity.insert(SceneRoot((*scene).clone()));
                                    }
                                }
                            }
                            None => match glxf_asset.nodes {
                                Some(_) => {
                                    warn!("`nodes` glXF asset property not implemented yet");
                                }
                                None => {
                                    if let Some(ref default_scene) = loaded_asset.default_scene {
                                        new_entity.insert(SceneRoot((*default_scene).clone()));
                                    }
                                }
                            },
                        },
                    }
                }
            };
        }

        // Add custom components, via the `BEVY_components` extension.
        if let Some(components_value) = node.extensions.get("BEVY_components") {
            match components_value.as_object() {
                None => {
                    error!("`BEVY_components` must be an object");
                }
                Some(components_object) => {
                    let type_registry = self.type_registry.read();
                    for (type_path, component_value) in components_object.iter() {
                        match type_registry.get_with_type_path(type_path) {
                            None => {
                                warn!("`{}` is not a registered type", type_path);
                            }
                            Some(type_registration) => {
                                match type_registration.data::<ReflectDeserialize>() {
                                    None => {
                                        // If there's no `ReflectDeserialize`,
                                        // try deserializing it using
                                        // `TypedReflectDeserializer`.
                                        let typed_reflect_deserializer =
                                            TypedReflectDeserializer::new(
                                                type_registration,
                                                &type_registry,
                                            );
                                        match typed_reflect_deserializer
                                            .deserialize(component_value)
                                        {
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
                                                    &mut new_entity,
                                                    type_registration,
                                                    &type_registry,
                                                    type_path,
                                                );
                                            }
                                        }
                                    }

                                    Some(reflect_deserialize) => {
                                        match reflect_deserialize.deserialize(component_value) {
                                            Err(error) => {
                                                error!(
                                                    "Failed to deserialize instance of `{}`: {:?}",
                                                    type_path, error
                                                );
                                            }
                                            Ok(component) => {
                                                add_component_to_entity(
                                                    component.as_partial_reflect(),
                                                    &mut new_entity,
                                                    type_registration,
                                                    &type_registry,
                                                    type_path,
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Add children.
        let new_entity = new_entity.id();
        if let Some(ref children) = node.children {
            for &kid in children {
                self.load_node(kid)?;
                if let Some(kid) = self.node_index_to_entity.get(&kid) {
                    self.world.entity_mut(new_entity).add_child(*kid);
                }
            }
        }

        self.node_index_to_entity.insert(node_index, new_entity);
        Ok(())
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
