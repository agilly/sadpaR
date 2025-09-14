# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Camera Trap Tagging Inspector is a Flask-based web application for reviewing and correcting camera trap tags. It provides an intuitive interface for inspecting tagged species and sequences in camera trap datasets.

## Development Commands

### Running the Application

```bash
# Primary method - cross-platform runner with auto-setup
python run.py

# Alternative methods
python app.py                    # Direct Flask execution
./start.sh                      # Unix/Linux/WSL
start.bat                       # Windows batch file
```

### Dependencies

```bash
# Install dependencies
pip install -r requirements.txt

# Core dependencies: Flask==2.3.3, pandas, Werkzeug==2.3.7, Jinja2==3.1.2
```

### Testing and Development

- No formal test suite is configured
- Application runs in debug mode when executed directly via `app.py`
- Production mode when run via `run.py`
- Local development server runs on `http://127.0.0.1:5000`

## Architecture Overview

### Core Application Structure

- **`app.py`**: Main Flask application with all routes and business logic
- **`run.py`**: Production runner that handles dependency installation and browser opening
- **`config.py`**: Configuration constants for validation, file formats, and CSV mappings
- **`templates/`**: Jinja2 HTML templates using Bootstrap 5 for UI
- **`requirements.txt`**: Python dependencies

### Key Architecture Patterns

**Data Flow**:
1. Dataset validation → CSV loading → In-memory pandas DataFrames
2. Cross-platform directory browsing with web-based file picker
3. Species-centric navigation (browse by species to see sequences)
4. Dual viewing modes: GIF sequences and individual photos

**Route Structure**:
- `/` - Dataset selector or redirect to main interface
- `/main` - Species list with event counts
- `/species/<id>` - Sequences for specific species
- `/sequence/<ctid>/<interval>` - Sequence viewer (GIF + individual photos)
- `/serve_gif/` and `/serve_image/` - Static file serving with security checks

**Data Management**:
- Global `dataset_info` dictionary stores all loaded CSV data as pandas DataFrames
- Dataset validation ensures required folder structure (`/sequences`, `/metadata`) and CSV files
- Photo path resolution handles different root path configurations
- CTID (Camera Trap ID) handling with multiple folder name formats (spaces → dots/underscores)

### Expected Dataset Structure

```
/dataset-root/
├── sequences/              # GIF sequences by Station.CTID
│   └── Station.CTID/
│       └── sequence.{i}.gif
├── metadata/               # Required CSV files
│   ├── species.csv
│   ├── intervals.csv
│   ├── stations.csv
│   ├── ct.csv
│   └── metadata.csv
└── tagging/               # Optional tagging data
    ├── eventTagging.csv
    ├── multipleEventTags.csv
    └── multipleEventStatus.csv
```

### Security Considerations

- Local-only application (`127.0.0.1`) not designed for public deployment
- Path traversal protection in file serving routes
- Secure filename handling with path validation against dataset root
- No authentication - intended for single-user local use

### Cross-Platform Compatibility

- Windows drive detection and WSL `/mnt` support in directory browser
- Platform-specific Python command detection in shell scripts
- Path normalization for different OS path separators
- File path handling for both absolute and relative paths

## Template Architecture

Templates use Bootstrap 5 with embedded CSS:
- **`base.html`**: Common layout with navigation and zoom functionality
- **`dataset_selector.html`**: Web-based directory browser
- **`main_interface.html`**: Species list with counts
- **`species_sequences.html`**: Sequence list for selected species
- **`sequence_viewer.html`**: GIF viewer with photo navigation and tagging sidebar

## Data Processing Notes

- Species IDs are converted to integers with NA value filtering
- CTID matching handles various folder naming conventions (spaces, dots, underscores)
- Photo root path auto-detection by comparing intervals.csv and multipleEventTags.csv
- Status tracking supports: complete, wip, attention, unknown
- Cross-referencing between CSV files using species IDs and CTIDs