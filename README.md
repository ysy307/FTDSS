# Freezing Thawing Dynamics Simulation Solver

## About

This application

### 📋 Project Info

[![License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](./LICENSE)

**Environment:** ![Docker](https://img.shields.io/badge/-Docker-EEE.svg?logo=docker&style=flat) ![Ubuntu](https://img.shields.io/badge/-Ubuntu%2022.04-EEE.svg?logo=ubuntu&style=flat)  
**Compiler:** ![ifx](https://img.shields.io/badge/ifx-2025.2.0-0071C5.svg?logo=intel&style=flat)  
**Language:** ![Fortran](https://img.shields.io/badge/Fortran-734f96.svg?logo=fortran&style=flat) ![C](https://img.shields.io/badge/C-00599C.svg?logo=c&style=flat) ![C++](https://img.shields.io/badge/C++-00599C.svg?logo=cplusplus&style=flat)  
**Editor:** ![VS Code](https://img.shields.io/badge/VS%20Code-2e8edb.svg?logo=visualstudiocode&style=flat)

## Goals and Motivations

## Scope

## Development Environment Setup & Compilation

Follow the steps below to set up your development environment and compile the project.

---

### 0. Configure Git (First Time Only)

Before committing, configure your Git username and email for this repository:

```bash
git config --local user.name "Your Name"
git config --local user.email "your.email@example.com"
```

This ensures Git commits inside the container will use your name and email.

> ℹ️ If you're using a **Dev Container**, you may also bind your local `.gitconfig` into the container so it applies globally inside the container as well.

In your `.devcontainer/devcontainer.json`, include the following under `mounts`:

```json
{
  "mounts": [
    "source=${localEnv:USERPROFILE}/.gitconfig,target=/root/.gitconfig,type=bind,consistency=cached"
  ]
}
```

This allows Git inside the container to inherit your global Git config (e.g., aliases, signing, default email).

---

### 1. Clone the Repository 📂

Clone the repository to your local machine:

```zsh
git clone https://github.com/ysy307/FTDSS.git
```

---

### 2. Launch with VS Code & Dev Containers 🐳

Use VS Code and Dev Containers to launch the development environment.

1. Open the cloned `FTDSS` folder in **VS Code**  
2. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS) to open the Command Palette  
3. Select **`Dev Containers: Reopen in Container`**  
4. VS Code will automatically build the Docker image and reopen the project inside the container

---

### 3. Install External Libraries 🛠️

Run the provided scripts to install the required external libraries:

```bash
# Grant execute permissions
chmod +x scripts/Install_VTKFortran.sh
chmod +x scripts/Install_VTK_CXX.sh
chmod +x scripts/Install_stdlib.sh
chmod +x scripts/Install_JsonFortran.sh

# Execute the scripts
./scripts/Install_VTKFortran.sh
./scripts/Install_VTK_CXX.sh
./scripts/Install_stdlib.sh
./scripts/Install_JsonFortran.sh
```

---

### 4. Compile the Project ⚙️

#### 4.1. Generate Build Files with CMake

```bash
cmake -S . -B CMakeBuild -DBUILD_APP=test -DCMAKE_BUILD_TYPE=Release -G "Ninja"
```

#### 4.2. Build the Project

```bash
cmake --build CMakeBuild --parallel
```

---

### 5. Run the Application ▶️

Run the compiled test application:

```bash
./bin/test
```

### Software environment

* Fortran 90 and later
* Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2025.2.0 Build 20250605
* CMake version 4.0.3
* Fortran Standard Library 0.7.0
* JSON-Fortran 9.0.3
* VTKFortran 2.0.3
* VTK - The Visualization Toolkit 9.5.0
* [IAPWS 0.0.1](https://github.com/ysy307/IAPWS.git)

## Usage

## Contributions

## Links

[Documents](https://ysy307.github.io/FTDSS/)

<!-- bfe41abf0975# code --install-extension /root/fortran-lang.linter-gfortran-3.2.0.vsix  -->
<!-- 開発コンテナー: FTDSS @ desktop-linux に拡張機能をインストールしています... -->
<!-- 拡張機能 'fortran-lang.linter-gfortran-3.2.0.vsix' が正常にインストールされました。 -->