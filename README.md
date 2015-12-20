
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

| Demo | Image |
|------|-------|
| Basic shapes with ambient occlusion | <a href="demos/demo1.png"><img src="/demos/demo.png" width="150" height="150"/></a> |
| Thin-lens depth of field            | <a href="demos/demo2.png"><img src="/demos/demo2.png" width="150" height="150"/></a> |
| Stanford Bunny                      | <a href="demos/demo3.png"><img src="/demos/demo3.png" width="150" height="150"/></a> |
| Dragon mesh                         | <a href="demos/demo4.png"><img src="/demos/demo4.png" width="150" height="150"/></a> |
| Area lights                         | <a href="demos/demo5.png"><img src="/demos/demo5.png" width="150" height="150"/></a> |
| Specular reflection                 | <a href="demos/demo6.png"><img src="/demos/demo6.png" width="150" height="150"/></a> |
| Path-traced table and chairs        | <a href="demos/demo8.png"><img src="/demos/demo8.png" width="150" height="150"/></a> |
| Blended materials                   | <a href="demos/demo9.png"><img src="/demos/demo9.png" width="150" height="150"/></a> |
| Textures                            | <a href="demos/demo10.png"><img src="/demos/demo10.png" width="150" height="150"/></a> |
