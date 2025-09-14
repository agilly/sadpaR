# Camera Trap Tagging Inspector

A web-based application for reviewing and correcting camera trap tags. This tool provides an intuitive interface for inspecting tagged species and sequences in camera trap datasets.

## Features

- **Dataset Validation**: Automatically validates dataset structure and required files
- **Web-Based Directory Browser**: Clean, cross-platform directory selection interface
- **Species-Oriented Navigation**: Browse by species to see all their tagged events
- **Dual View Modes**: 
  - GIF View: See animated sequences
  - Photo View: Navigate through individual photos with arrow keys
- **Smart Image Handling**: Click-to-zoom functionality for detailed inspection
- **Tagging Information**: View detailed tagging data for sequences and individual photos
- **Status Tracking**: See completion status of tagged sequences
- **Cross-Platform**: Works consistently on Windows, macOS, and Linux (including WSL)

## Requirements

- Python 3.7+
- Web browser (Chrome, Firefox, Safari, etc.)

## Dataset Structure

The application expects the following dataset structure:

```
/your-dataset/
├── sequences/          # GIF sequences organized by Station.CTID
│   └── Station.CTID/
│       └── sequence.{i}.gif
├── metadata/           # Required CSV files
│   ├── species.csv
│   ├── intervals.csv
│   ├── stations.csv
│   ├── ct.csv
│   └── metadata.csv
└── tagging/           # Optional tagging data
    ├── eventTagging.csv
    ├── multipleEventTags.csv
    └── multipleEventStatus.csv
```

## Quick Start

1. **Navigate to the application directory**:
   ```bash
   cd sadpaR/taggingInspector
   ```

2. **Run the application**:
   ```bash
   python run.py
   ```

3. **Open your browser** (should open automatically) and go to:
   ```
   http://127.0.0.1:5000
   ```

4. **Select your dataset** using the web-based directory browser:
   - Navigate through directories using the intuitive interface
   - Directories with valid camera trap datasets will be highlighted with a camera icon
   - Select the desired dataset directory and click "Load Dataset"

5. **Browse species** and inspect their tagged sequences

## Usage

### Main Interface
- View all tagged species with event counts
- Click "View" next to any species to see their sequences

### Directory Selection
- **Web-Based Browser**: Navigate directories with a clean, modern interface
- **Cross-Platform Consistency**: Works the same on Windows, Linux, and macOS
- **Dataset Detection**: Automatically highlights valid camera trap datasets
- **Path Navigation**: Use breadcrumbs and up/refresh buttons for easy navigation

### Species Sequences
- See all sequences containing a particular species
- View status (complete, wip, attention) of each sequence
- Access individual sequence viewers

### Sequence Viewer
- **GIF View**: See the animated sequence with zoom functionality
- **Photo View**: Navigate through individual photos with:
  - Arrow keys for navigation
  - Click to zoom in/out
  - Species tags shown below each photo
- View detailed tagging information in the sidebar

### Navigation
- Use breadcrumbs to navigate back through the interface
- Keyboard shortcuts in Photo View:
  - ← → Arrow keys to navigate photos
  - Click images to zoom

## File Structure

```
taggingInspector/
├── app.py              # Main Flask application
├── run.py              # Application runner script
├── requirements.txt    # Python dependencies
├── README.md          # This file
└── templates/         # HTML templates
    ├── base.html
    ├── dataset_selector.html
    ├── main_interface.html
    ├── species_sequences.html
    └── sequence_viewer.html
```

## Dependencies

- **Flask**: Web framework
- **pandas**: Data manipulation and CSV handling
- **platform**: OS detection for cross-platform compatibility

## Troubleshooting

### Dataset Not Loading
- Ensure all required folders (`/sequences`, `/metadata`) exist
- Check that required CSV files are present in `/metadata`
- Verify CSV files have the expected column headers

### Images Not Displaying
- Check that photo paths in `intervals.csv` are accessible
- The application will attempt to determine the correct photo root path
- Verify GIF files exist in the `/sequences` folder structure

### Performance with Large Images
- Large images are handled with smart loading and zoom functionality
- Use the zoom feature instead of trying to load very high resolution images at full size

## Development

To modify or extend the application:

1. **Backend**: Edit `app.py` for data processing and routing
2. **Frontend**: Modify templates in the `templates/` folder
3. **Styling**: CSS is embedded in the templates using Bootstrap 5

## Security Note

This application is designed for local use only. It provides direct file system access and should not be deployed to a public server without additional security measures.
