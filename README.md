
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
 * Light sources:
   - Point sources
   - Ambient light
 * User interface:
   - Console output with status information
   - Live GUI with in-progress rendering via GLUT
 * Engine:
   - Multi-core rendering via Haskell's parMap evaluation strategy
   - Control over per-pixel sample rate
 * Other:
   - Sampler debugging program to test sample distribution for various
     sampling methods

![demo](/demo.png)
