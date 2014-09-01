
tracy - a Haskell ray tracer
============================

Features:

 * Basic idealized primitives:
   - Triangles
   - Spheres
   - Cubes
   - Planes
 * Acceleration aids:
   - Regular grids
 * Materials and shading:
   - Phong shading
 * Shadows and effects:
   - Basic shadows
   - Ambient occlusion
 * Cameras:
   - Thin-lens camera
 * Light sources:
   - Point sources
   - Ambient light
 * User interface:
   - Console output with status information
   - Live GUI with in-progress rendering via GLUT
 * Engine:
   - Multi-core rendering via Haskell's parMap evaluation strategy
   - Multi-host rendering (use the `tracy-node` program and pass `-d` to `tracy`)
   - Control over per-pixel sample rate
 * Other:
   - Sampler debugging program to test sample distribution for various
     sampling methods

Basic shapes with ambient occlusion:
![demo](/demo.png)

Thin-lens view with specular highlights and ambient occlusion:
![demo2](/demo2.png)
