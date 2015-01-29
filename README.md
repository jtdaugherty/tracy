
tracy - a Haskell ray tracer
============================

Features:

 * Basic idealized primitives:
   - Triangles
   - Spheres (and concave spheres with inverted normals for scene
     lighting)
   - Boxes
   - Planes
   - Rectangles
   - Triangle meshes
 * Triangle mesh shading:
   - Flat
   - Smooth shading (if vertex normals are available in mesh data)
 * Triangle mesh file formats:
   - PLY
 * Acceleration aids:
   - Regular grids
 * Materials and shading:
   - Phong
   - Matte
   - Perfect specular reflection
   - Glossy specular reflection
   - Mix (blended materials)
 * Object instancing
 * Object transformations
 * Global illumation:
   - Ambient occlusion (approximation technique)
   - Path-tracing support
 * Cameras:
   - Thin-lens camera
 * Light source types:
   - Ambient
   - Point (with hard shadows)
   - Area lights (supported geometry: rectangles with area-light tracer,
     any geometry with path tracer)
   - Environment lights
 * User interface:
   - Console output with status information
   - Live GUI with progressive rendering
 * Scene representation:
   - YAML-based file format
   - Network (de)serialization
 * Animation:
   - Renders a selected frame from an animation sequence (frames can be
     stitched into movies using external tools; see `scripts/`)
   - Linear interpolation of float values, rotational interpolation of
     vectors for some object fields
 * Engine:
   - Multi-core rendering via Haskell's `parMap` evaluation strategy
   - Multi-host rendering (run `tracy -d tcp://slave:9000` on the master,
     `tracy-node` on the slave(s))
 * Other:
   - Sampler debugging program to test sample distribution for various
     sampling methods

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
