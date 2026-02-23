title: FTDSS Documentation
author: Kikuchi Shun
---

FTDSS (Freezing/Thawing Dynamics Soil Solver)
=============================================

FTDSS is a Fortran-based numerical solver designed to simulate freezing and thawing processes in soil, integrating heat and water transport (TH coupling).

Features
--------
* **Physical Modeling**: Simulates phase change in soil using the enthalpy method.
* **Spatial Discretization**: Finite Element Method (FEM).
* **Nonlinear Iteration**: Picard and Newton-Raphson methods.
* **Linear Solvers**: Iterative solvers including BiCGSTAB and GMRES with preconditioners (e.g., ILU).

Documentation Structure
-----------------------
Use the navigation bar to browse the API documentation:
* **Modules**: Module specifications and dependencies.
* **Procedures**: Subroutine and function interfaces.
* **Types**: Derived type definitions and methods.
* **Source Files**: Directory structure and file descriptions.

Build Requirements
------------------
* Fortran compiler
* CMake
* fypp (Fortran preprocessor)

For more details, visit the [GitHub repository](https://github.com/ysy307/FTDSS.git).