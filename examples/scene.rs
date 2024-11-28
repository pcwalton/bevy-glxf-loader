// bevy-glxf-loader/examples/scene.rs

use bevy::{
    app::Startup,
    asset::AssetServer,
    math::Vec3,
    prelude::{App, Camera3d, Commands, Res, Transform},
    scene::{SceneBundle, SceneRoot},
    utils::default,
    DefaultPlugins,
};
use bevy_glxf_loader::GlxfPlugin;
use bevy_inspector_egui::quick::WorldInspectorPlugin;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(GlxfPlugin)
        .add_plugins(WorldInspectorPlugin::new())
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(SceneRoot(asset_server.load("Scene.glxf#Scene0")));

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(-2.5, 4.5, 9.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));
}
