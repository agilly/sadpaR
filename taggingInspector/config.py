# Camera Trap Tagging Inspector Configuration
# This file contains configuration options for the application

# Server Configuration
HOST = "127.0.0.1"  # Local host only for security
PORT = 5000         # Port to run the application on
DEBUG = False       # Set to True for development

# Image Display Configuration
MAX_IMAGE_SIZE = 2048  # Maximum image dimension for display (pixels)
ZOOM_LEVELS = [1, 2, 3, 4]  # Available zoom levels

# Dataset Validation
REQUIRED_FOLDERS = ["sequences", "metadata"]
REQUIRED_FILES = [
    "metadata/species.csv",
    "metadata/intervals.csv", 
    "metadata/stations.csv",
    "metadata/ct.csv",
    "metadata/metadata.csv"
]

OPTIONAL_FOLDERS = ["tagging"]
OPTIONAL_FILES = [
    "tagging/eventTagging.csv",
    "tagging/multipleEventTags.csv",
    "tagging/multipleEventStatus.csv"
]

# CSV Column Mappings
SPECIES_COLUMNS = {
    "id": "id",
    "common_name": "Common Name",
    "lao_name": "Lao Name", 
    "species_name": "Species Name",
    "group": "Group",
    "family": "Family",
    "order": "Order"
}

INTERVALS_COLUMNS = {
    "filename": "fn",
    "datetime": "dt",
    "location": "location",
    "camera": "ct",
    "interval": "interval", 
    "ctid": "ctid"
}

# File Extensions
SUPPORTED_IMAGE_FORMATS = [".jpg", ".jpeg"]
SUPPORTED_VIDEO_FORMATS = [".gif",]
