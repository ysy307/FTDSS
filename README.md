# Freezing Thawing Dynamics Simulation Solver

## About
This application

### 📋 Project Info

| License | Environment | Compiler Support | Language | Editor |
|---------|-------------|------------------|----------|--------|
| [![License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](./LICENSE) | ![Docker](https://img.shields.io/badge/-Docker-EEE.svg?logo=docker&style=flat) <br> ![Ubuntu](https://img.shields.io/badge/-Ubuntu%2022.04-EEE.svg?logo=ubuntu&style=flat) | ![ifx](https://img.shields.io/badge/ifx-2025.2.0-0071C5.svg?logo=intel&style=flat) | ![Fortran](https://img.shields.io/badge/Fortran-734f96.svg?logo=fortran&style=flat) <br> ![C](https://img.shields.io/badge/C-00599C.svg?logo=c&style=flat) <br> ![C++](https://img.shields.io/badge/C++-00599C.svg?logo=cplusplus&style=flat) | ![VS Code](https://img.shields.io/badge/VS%20Code-2e8edb.svg?logo=visualstudiocode&style=flat) |

## Goals and Motivations

## Scope

## How to Install
Development Environment Setup & Compilation (Markdown Source)  
This guide shows the Markdown source for documenting the setup and compilation workflow.

### 1. Clone the Repository 📂  
To create a code block for cloning the repository, you would write the following in Markdown:

```zsh
git clone https://github.com/ysy307/FTDSS.git
```

### 2. Launch with VS Code & Dev Containers 🐳  
This section explains how to launch the development environment. It uses numbered lists and bold text.

1. Open the cloned `FTDSS` folder in **VS Code**  
2. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS) to open the Command Palette and select **`Dev Containers: Reopen in Container`**  
3. VS Code will automatically build the Docker image and reopen the project inside the container.

### 3. Install External Libraries 🛠️  
For the library installation steps, you can create a code block that includes comments:

```bash
# Add execute permissions
chmod +x Scripts/Install_VTKFortran.sh
chmod +x Scripts/Install_VTK_CXX.sh
chmod +x Scripts/Install_stdlib.sh
chmod +x Scripts/Install_JsonFortran.sh

# Run each script
./Scripts/Install_VTKFortran.sh
./Scripts/Install_VTK_CXX.sh
./Scripts/Install_stdlib.sh
./Scripts/Install_JsonFortran.sh
```

### 4. Compile the Project ⚙️

#### 4.1. Generate Build Files (CMake)

```bash
cmake -S . -B CMakeBuild -DBUILD_APP=test -DCMAKE_BUILD_TYPE=Release -G "Ninja"
```

#### 4.2. Run the Build

```bash
cmake --build CMakeBuild --parallel
```

### 5. Run the Application ▶️

```bash
./bin/test
```

### Software environment
* Fortran 90 and later
* Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2025.2.0 Build 20250605
* CMake version 4.0.3
* GNU Make 4.3
* Fortran Standard Library 0.7.0
* JSON-Fortran 9.0.3
* VTKFortran 2.0.3
* VTK - The Visualization Toolkit 9.5.0


## Usage

## Contributions

## Links
[Documents](https://ysy307.github.io/FTDSS/)


<!-- bfe41abf0975# code --install-extension /root/fortran-lang.linter-gfortran-3.2.0.vsix  -->
<!-- 開発コンテナー: FTDSS @ desktop-linux に拡張機能をインストールしています... -->
<!-- 拡張機能 'fortran-lang.linter-gfortran-3.2.0.vsix' が正常にインストールされました。 -->