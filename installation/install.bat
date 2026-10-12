@echo off
setlocal
echo ============================================
echo          MeDUSA Installer (Windows)
echo ============================================
echo.

:: Check for Rscript in PATH first
where Rscript >nul 2>nul
if %errorlevel% equ 0 (
    set "RSCRIPT=Rscript"
    echo Found R at: %RSCRIPT%
    goto :run
)

:: Search common R install locations
echo R not found in PATH. Searching common install locations...
set "RSCRIPT="

:: Check Program Files
for /d %%D in ("C:\Program Files\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
)

:: Check Program Files (x86)
if not defined RSCRIPT (
    for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

:: Check user-level install in AppData
if not defined RSCRIPT (
    for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

if not defined RSCRIPT (
    echo.
    echo ERROR: R could not be found on this system.
    echo.
    echo Please install R from: https://cran.r-project.org/
    echo.
    pause
    exit /b 1
)

echo Found R at: %RSCRIPT%

:run
echo Starting installation...
echo.

:: Go to parent folder where MeDUSA lives
pushd "%~dp0\.."

:: Run R file
"%RSCRIPT%" "%~dp0\install.R" %*
set "EXIT_CODE=%errorlevel%"

:: Return to original folder
popd

echo.
if not "%EXIT_CODE%"=="0" (
    echo Installation encountered errors. Please check the messages above.
    echo.
    pause
    exit /b %EXIT_CODE%
) else (
    echo Installation successful!
    echo.
    pause
)


