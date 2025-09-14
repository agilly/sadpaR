#!/bin/bash

echo "Camera Trap Tagging Inspector"
echo "============================="
echo

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    if ! command -v python &> /dev/null; then
        echo "Error: Python is not installed or not in PATH"
        echo "Please install Python 3.7+ and try again"
        exit 1
    else
        PYTHON_CMD="python"
    fi
else
    PYTHON_CMD="python3"
fi

# Check if we're in the right directory
if [ ! -f "app.py" ]; then
    echo "Error: app.py not found in current directory"
    echo "Please run this script from the taggingInspector directory"
    exit 1
fi

echo "Starting application..."
echo "The web interface will open in your default browser."
echo "Press Ctrl+C to stop the application."
echo

$PYTHON_CMD run.py
