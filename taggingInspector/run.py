#!/usr/bin/env python3
"""
Camera Trap Tagging Inspector Runner
Simple script to start the web application
"""

import sys
import os
import subprocess
import webbrowser
import time
import threading

def install_requirements():
    """Install required packages"""
    print("Installing required packages...")
    try:
        subprocess.check_call([sys.executable, "-m", "pip", "install", "-r", "requirements.txt"])
        print("✓ Requirements installed successfully")
        return True
    except subprocess.CalledProcessError as e:
        print(f"✗ Error installing requirements: {e}")
        return False

def start_app():
    """Start the Flask application"""
    print("Starting Camera Trap Tagging Inspector...")
    print("The web application will open in your default browser.")
    print("If it doesn't open automatically, go to: http://127.0.0.1:5000")
    print("Press Ctrl+C to stop the application.")
    
    # Import and run the app
    try:
        from app import app
        
        # Open browser after a short delay
        def open_browser():
            time.sleep(1.5)
            try:
                webbrowser.open('http://127.0.0.1:5000')
            except:
                # Browser opening might fail in some environments (like WSL)
                pass
        
        threading.Thread(target=open_browser, daemon=True).start()
        
        # Start the Flask app
        app.run(debug=False, host='127.0.0.1', port=5000)
        
    except ImportError as e:
        print(f"✗ Error importing app: {e}")
        print("Make sure all requirements are installed.")
        return False
    except KeyboardInterrupt:
        print("\n✓ Application stopped by user")
        return True
    except Exception as e:
        print(f"✗ Error starting application: {e}")
        return False

def main():
    """Main entry point"""
    print("Camera Trap Tagging Inspector")
    print("=" * 40)
    
    # Check if we're in the right directory
    if not os.path.exists('app.py'):
        print("✗ Error: app.py not found in current directory")
        print("Please run this script from the taggingInspector directory")
        sys.exit(1)
    
    # Install requirements if needed
    if not install_requirements():
        sys.exit(1)
    
    # Start the application
    if not start_app():
        sys.exit(1)

if __name__ == '__main__':
    main()
