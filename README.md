
tracy - a Haskell ray tracer
============================

Features:

 * Basic idealized primitives:
   - Triangles
   - Spheres
   - Cubes
   - Planes
 * Flat-shaded triangle meshes
 * Triangle mesh file formats:
   - PLY
 * Acceleration aids:
   - Regular grids
 * Materials and shading:
   - Phong
   - Matte
 * Global illumation:
   - Ambient occlusion (approximation technique)
 * Cameras:
   - Thin-lens camera
 * Light source types:
   - Ambient
   - Point (with hard shadows)
 * User interface:
   - Console output with status information
   - Live GUI with in-progress rendering via GLUT
 * Engine:
   - Multi-core rendering via Haskell's `parMap` evaluation strategy
   - Multi-host rendering (run `tracy -d tcp://slave:9000` on the master,
     `tracy-node` on the slave(s))
 * Other:
   - Sampler debugging program to test sample distribution for various
     sampling methods

Basic shapes with ambient occlusion:
![demo](/demo.png)

Thin-lens view with specular highlights and ambient occlusion:
![demo2](/demo2.png)

Stanford Bunny mesh:
![demo3](/demo3.png)

Dragon mesh:
![demo4](/demo4.png)
