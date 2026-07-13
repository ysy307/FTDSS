@echo off
setlocal

set "ONEAPI_URL=%~1"
set "ONEAPI_COMPONENTS=%~2"
set "ONEAPI_INSTALLER=%TEMP%\oneapi-installer.exe"

curl.exe --fail --location --retry 5 --retry-delay 5 --output "%ONEAPI_INSTALLER%" "%ONEAPI_URL%"
if errorlevel 1 exit /b 1

start "" /b /wait "%ONEAPI_INSTALLER%" -s -x -f oneapi-extracted --log extract.log
if errorlevel 1 exit /b 1

oneapi-extracted\bootstrapper.exe -s --action install --components=%ONEAPI_COMPONENTS% --eula=accept ^
    -p=NEED_VS2022_INTEGRATION=0 --log-dir=.
exit /b %ERRORLEVEL%
