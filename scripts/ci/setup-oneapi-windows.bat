@echo off

set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
if not exist "%VSWHERE%" (
    echo ERROR: vswhere.exe was not found.
    exit /b 1
)

set "VS_INSTALL_PATH="
set "VS_INSTALL_VERSION="
for /f "usebackq delims=" %%I in (`"%VSWHERE%" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do set "VS_INSTALL_PATH=%%I"
for /f "usebackq delims=" %%I in (`"%VSWHERE%" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationVersion`) do set "VS_INSTALL_VERSION=%%I"

if not defined VS_INSTALL_PATH (
    echo ERROR: A Visual Studio installation with the C++ toolchain was not found.
    exit /b 1
)

if "%VS_INSTALL_VERSION:~0,3%"=="16." goto setup_vs2019
if "%VS_INSTALL_VERSION:~0,3%"=="17." goto setup_vs2022
if "%VS_INSTALL_VERSION:~0,3%"=="18." goto setup_vs2026

echo ERROR: Unsupported Visual Studio version %VS_INSTALL_VERSION% at "%VS_INSTALL_PATH%".
exit /b 1

:setup_vs2019
set "VS2019INSTALLDIR=%VS_INSTALL_PATH%"
goto setup_oneapi

:setup_vs2022
set "VS2022INSTALLDIR=%VS_INSTALL_PATH%"
goto setup_oneapi

:setup_vs2026
set "VS2026INSTALLDIR=%VS_INSTALL_PATH%"
goto setup_oneapi

:setup_oneapi
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
exit /b %ERRORLEVEL%
