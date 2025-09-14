@echo off
echo Camera Trap Tagging Inspector
echo =============================
echo.

REM Check if Python is available
python --version >nul 2>&1
if errorlevel 1 (
    echo Error: Python is not installed or not in PATH
    echo Please install Python 3.7+ and try again
    pause
    exit /b 1
)

REM Check if we're in the right directory
if not exist "app.py" (
    echo Error: app.py not found in current directory
    echo Please run this script from the taggingInspector directory
    pause
    exit /b 1
)

echo Starting application...
echo The web interface will open in your default browser.
echo Press Ctrl+C to stop the application.
echo.

python run.py

pause
