@echo off
setlocal
echo ============================================
echo          MeDUSA Uninstaller (Windows)
echo ============================================
echo.

:: Check for Rscript in PATH first
where Rscript >nul 2>nul
if %errorlevel% equ 0 (
    set "RSCRIPT=Rscript"
    goto :run
)

:: Search common R install locations
set "RSCRIPT="
for /d %%D in ("C:\Program Files\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
)
if not defined RSCRIPT (
    for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)
if not defined RSCRIPT (
    for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

if not defined RSCRIPT (
    echo ERROR: R could not be found on this system.
    pause
    exit /b 1
)

:: Set SCRIPT_DIR to path to batch file (2026-34-MeDUSA/installation/) and PROJECT_ROOT to parent MeDUSA file
:run
set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "PROJECT_ROOT=%%~fI"

echo Starting de-installation...
echo Project root: %PROJECT_ROOT%
echo.

set "RENV_BOOTSTRAP=false"
set "RENV_CONSENT=true"

:: Go to the project root
pushd "%PROJECT_ROOT%"
"%RSCRIPT%" "%SCRIPT_DIR%uninstall.R" %*
set EXIT_CODE=%errorlevel%
popd

echo.
if not "%EXIT_CODE%"=="0" (
    echo De-installation encountered errors. Please check the messages above.
) else (
    echo De-installation successful!
)

echo.
pause
exit /b %EXIT_CODE%

