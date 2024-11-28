# `bevy-glxf-loader`

`bevy-glxf-loader` is a simple loader for the [glXF] format (previously known as glTFX). glXF is a very simple JSON-based format that allows you to arrange glTF and glXF scenes into a larger scene (or "experience"). `bevy-glxf-loader` implements a `BEVY_components` extension, allowing you to add arbitrary components to any of your objects.

Essentially, `bevy-glxf-loader` is a workaround for the current lack of [BSN]. It allows you to specify your scenes as an asset as opposed to in code, allowing for faster iteration times and the potential for better tooling.

See the `scene` example for an example of usage. The `Scene.glxf` and `Subscene.glxf` files under `assets` demonstrate how to use the glXF format with the `BEVY_components` extension.

## Known limitations

* Extracting individual nodes from an asset (the `nodes` property in `assets`) isn't supported yet.

## License

Dual-licensed under the MIT and Apache 2 licenses. Choose one of the two licenses, at your option.

## Code of conduct

`bevy-glxf-loader` follows the same code of conduct as Rust itself. Reports can be made to the project authors.

[glXF]: https://github.com/KhronosGroup/glTF-External-Reference

[BSN]: https://github.com/bevyengine/bevy/discussions/14437
