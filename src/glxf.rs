//! The glXF DOM.

use bevy::{asset::Asset as BevyAsset, reflect::Reflect};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Clone, Debug, Default, Serialize, Deserialize, BevyAsset, Reflect)]
pub struct Glxf {
    pub asset: AssetHeader,
    pub assets: Vec<Asset>,
    pub nodes: Vec<Node>,
    pub scenes: Vec<Scene>,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, Reflect)]
pub struct AssetHeader {
    pub version: String,
    #[serde(default)]
    pub experience: bool,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, Reflect)]
pub struct Asset {
    pub uri: String,
    #[serde(default)]
    pub scene: Option<String>,
    #[serde(default)]
    pub nodes: Option<Vec<String>>,
    #[serde(default)]
    pub transform: AssetTransform,
}

#[derive(Clone, Copy, Default, Debug, Serialize, Deserialize, Reflect)]
#[serde(rename_all = "lowercase")]
pub enum AssetTransform {
    None,
    Local,
    #[default]
    Global,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, Reflect)]
pub struct Scene {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub nodes: Option<Vec<u32>>,
}

#[derive(Clone, Debug, Serialize, Deserialize, Reflect)]
pub struct Node {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub children: Option<Vec<u32>>,
    #[serde(default)]
    pub asset: Option<u32>,
    #[serde(default)]
    pub matrix: Option<[[f32; 4]; 4]>,
    #[serde(default)]
    pub scale: Option<[f32; 3]>,
    #[serde(default)]
    pub translation: Option<[f32; 3]>,
    #[serde(default)]
    pub rotation: Option<[f32; 4]>,
    #[serde(default)]
    #[reflect(ignore)]
    pub extensions: HashMap<String, Value>,
}
