
tracy - a Haskell ray tracer
============================

Features:

 * Basic idealized primitives: triangles, spheres, concave spheres,
   boxes, planes, tori, rectangles, triangle meshes
 * Mesh shading modes: flat, smooth (when vertex normals are available),
   textured (when UV coordinates are available)
 * Mesh file formats: PLY
 * Acceleration aids: regular grids, bounding volume hierarchies
 * Materials and shading: phong, matte, perfect specular reflection,
   glossy specular reflection, blended materials
 * Image-based textures
 * Procedural textures: planar checkers, spherical checkers
 * Texture mappings: rectangular, tiled (planar), spherical
 * Object instancing
 * Object transformations
 * Samplers: regular, random, jittered, multi-jittered, correlated
   multi-jittered
 * Global illumation: ambient occlusion, path-tracing
 * Cameras: thin-lens
 * Light source types: ambient, point, area (rectangles only),
   environment
 * Console output with status information
 * Live GUI window with progressive rendering
 * YAML-based scene file format
 * Basic animation support for some scalar and vector parameters
 * Multi-core rendering engine using Haskell's `parMap` evaluation
   strategy
 * Network rendering (run `tracy -d tcp://slave:9000` on the master,
   `tracy-node` on the slave(s))

Basic shapes with ambient occlusion:
![demo](/demos/demo.png)

Thin-lens view with specular highlights and ambient occlusion:
![demo2](/demos/demo2.png)

Stanford Bunny mesh:
![demo3](/demos/demo3.png)

Dragon mesh:
![demo4](/demos/demo4.png)

Area lights:
![demo5](/demos/demo5.png)

Perfect and glossy specular reflection:
![demo6](/demos/demo6.png)

Table and chairs, path-traced:
![demo8](/demos/demo8.png)

Blended materials:
![demo9](/demos/demo9.png)
