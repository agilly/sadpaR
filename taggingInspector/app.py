"""
Camera Trap Tagging Inspector
A web application for reviewing and correcting camera trap tags.
"""

import os
import json
import pandas as pd
from flask import Flask, render_template, request, jsonify, send_file, redirect, url_for, session, flash
from werkzeug.utils import secure_filename
from urllib.parse import unquote
import platform
import threading

app = Flask(__name__)
app.config['SECRET_KEY'] = 'cameratrap_inspector_2025'

# Global variables to store dataset information
dataset_info = {
    'root_path': None,
    'photo_root': None,
    'species_df': None,
    'intervals_df': None,
    'stations_df': None,
    'ct_df': None,
    'metadata_df': None,
    'event_tagging_df': None,
    'multiple_event_tags_df': None,
    'multiple_event_status_df': None,
    'is_loaded': False
}

@app.route('/')
def index():
    """Main page - either dataset selection or main interface"""
    if not dataset_info['is_loaded']:
        return render_template('dataset_selector.html')
    else:
        return redirect(url_for('main_interface'))

@app.route('/select_dataset', methods=['POST'])
def select_dataset():
    """Handle dataset directory selection"""
    try:
        directory = request.json.get('directory')
        
        if not directory:
            return jsonify({'success': False, 'message': 'No directory provided'})
        
        if not os.path.exists(directory):
            return jsonify({'success': False, 'message': 'Directory does not exist'})
        
        # Validate dataset structure
        validation_result = validate_dataset(directory)
        if not validation_result['valid']:
            return jsonify({'success': False, 'message': validation_result['message']})
        
        # Load dataset
        load_result = load_dataset(directory)
        if not load_result['success']:
            return jsonify({'success': False, 'message': load_result['message']})
        
        return jsonify({'success': True, 'message': 'Dataset loaded successfully'})
    
    except Exception as e:
        return jsonify({'success': False, 'message': f'Error selecting dataset: {str(e)}'})

@app.route('/browse_directory')
def browse_directory():
    """Browse directories for dataset selection"""
    path = request.args.get('path', '')
    
    # Determine starting path based on OS
    if not path:
        if platform.system() == 'Windows':
            # On Windows, start with drives
            try:
                drives = ['%s:' % d for d in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' if os.path.exists('%s:' % d)]
                return jsonify({
                    'current_path': '',
                    'parent_path': None,
                    'items': [{'name': drive, 'type': 'drive', 'path': f'{drive}/'} for drive in drives]
                })
            except:
                path = 'C:/'
        else:
            # On Unix-like systems, start with common directories
            home_dir = os.path.expanduser('~')
            common_paths = [
                {'name': 'Home', 'type': 'directory', 'path': home_dir},
                {'name': 'Root', 'type': 'directory', 'path': '/'},
            ]
            # Add /mnt for WSL users
            if os.path.exists('/mnt'):
                common_paths.append({'name': 'Mounted Drives (/mnt)', 'type': 'directory', 'path': '/mnt'})
            
            return jsonify({
                'current_path': '',
                'parent_path': None,
                'items': common_paths
            })
    
    # Normalize the path
    path = os.path.abspath(path)
    
    if not os.path.exists(path):
        return jsonify({'error': 'Path does not exist'}), 400
    
    if not os.path.isdir(path):
        return jsonify({'error': 'Path is not a directory'}), 400
    
    try:
        items = []
        
        # Get parent directory
        parent_path = os.path.dirname(path) if path != os.path.dirname(path) else None
        
        # List directory contents
        for item_name in sorted(os.listdir(path)):
            item_path = os.path.join(path, item_name)
            
            if os.path.isdir(item_path):
                # Check if this looks like a camera trap dataset
                is_dataset = all(os.path.exists(os.path.join(item_path, folder)) 
                               for folder in ['sequences', 'metadata'])
                
                items.append({
                    'name': item_name,
                    'type': 'dataset' if is_dataset else 'directory',
                    'path': item_path
                })
        
        return jsonify({
            'current_path': path,
            'parent_path': parent_path,
            'items': items
        })
        
    except PermissionError:
        return jsonify({'error': 'Permission denied'}), 403
    except Exception as e:
        return jsonify({'error': str(e)}), 500

def validate_dataset(directory):
    """Validate that the dataset has the required structure"""
    required_folders = ['sequences', 'metadata']
    required_files = [
        'metadata/species.csv',
        'metadata/intervals.csv',
        'metadata/stations.csv',
        'metadata/ct.csv',
        'metadata/metadata.csv'
    ]
    
    # Check required folders
    for folder in required_folders:
        folder_path = os.path.join(directory, folder)
        if not os.path.exists(folder_path):
            return {'valid': False, 'message': f'Required folder missing: {folder}'}
    
    # Check required files
    for file_path in required_files:
        full_path = os.path.join(directory, file_path)
        if not os.path.exists(full_path):
            return {'valid': False, 'message': f'Required file missing: {file_path}'}
    
    # Check optional tagging folder
    tagging_path = os.path.join(directory, 'tagging')
    has_tagging = os.path.exists(tagging_path)
    
    return {'valid': True, 'has_tagging': has_tagging}

def determine_photo_root(directory):
    """Determine the root path for photos by comparing intervals.csv and multipleEventTags.csv"""
    intervals_path = os.path.join(directory, 'metadata', 'intervals.csv')
    multiple_tags_path = os.path.join(directory, 'tagging', 'multipleEventTags.csv')
    
    if not os.path.exists(multiple_tags_path):
        # If no tagging data, assume photos are at the paths in intervals.csv
        intervals_df = pd.read_csv(intervals_path)
        if not intervals_df.empty:
            sample_path = intervals_df['fn'].iloc[0]
            # Try to find the photo root
            if os.path.exists(sample_path):
                return os.path.dirname(sample_path)
            else:
                # Photo root might need to be adjusted
                return None
        return None
    
    # Compare paths in both files
    intervals_df = pd.read_csv(intervals_path)
    tags_df = pd.read_csv(multiple_tags_path)
    
    if intervals_df.empty or tags_df.empty:
        return None
    
    # Get sample paths
    intervals_path_sample = intervals_df['fn'].iloc[0]
    tags_path_sample = tags_df['fn'].iloc[0]
    
    # Find the difference to determine photo root
    if intervals_path_sample.endswith(tags_path_sample):
        photo_root = intervals_path_sample[:-len(tags_path_sample)]
        return photo_root.rstrip('/')
    
    return None

def load_dataset(directory):
    """Load all dataset files into memory"""
    try:
        dataset_info['root_path'] = directory
        
        # Load metadata files
        dataset_info['species_df'] = pd.read_csv(os.path.join(directory, 'metadata', 'species.csv'))
        # Ensure species IDs are integers, drop rows with NA values
        if 'id' in dataset_info['species_df'].columns:
            dataset_info['species_df'] = dataset_info['species_df'].dropna(subset=['id'])
            dataset_info['species_df']['id'] = dataset_info['species_df']['id'].astype(int)
        
        dataset_info['intervals_df'] = pd.read_csv(os.path.join(directory, 'metadata', 'intervals.csv'))
        dataset_info['stations_df'] = pd.read_csv(os.path.join(directory, 'metadata', 'stations.csv'))
        dataset_info['ct_df'] = pd.read_csv(os.path.join(directory, 'metadata', 'ct.csv'))
        dataset_info['metadata_df'] = pd.read_csv(os.path.join(directory, 'metadata', 'metadata.csv'))
        
        # Load tagging files if they exist
        tagging_path = os.path.join(directory, 'tagging')
        if os.path.exists(tagging_path):
            event_tagging_path = os.path.join(tagging_path, 'eventTagging.csv')
            multiple_tags_path = os.path.join(tagging_path, 'multipleEventTags.csv')
            status_path = os.path.join(tagging_path, 'multipleEventStatus.csv')
            
            if os.path.exists(event_tagging_path):
                dataset_info['event_tagging_df'] = pd.read_csv(event_tagging_path)
                # Ensure species IDs are integers in tagging data, drop NA values
                if 'speciesID' in dataset_info['event_tagging_df'].columns:
                    dataset_info['event_tagging_df'] = dataset_info['event_tagging_df'].dropna(subset=['speciesID'])
                    dataset_info['event_tagging_df']['speciesID'] = dataset_info['event_tagging_df']['speciesID'].astype(int)
            
            if os.path.exists(multiple_tags_path):
                dataset_info['multiple_event_tags_df'] = pd.read_csv(multiple_tags_path)
                # Ensure species IDs are integers in multiple event tags, drop NA values
                if 'species' in dataset_info['multiple_event_tags_df'].columns:
                    dataset_info['multiple_event_tags_df'] = dataset_info['multiple_event_tags_df'].dropna(subset=['species'])
                    dataset_info['multiple_event_tags_df']['species'] = dataset_info['multiple_event_tags_df']['species'].astype(int)
            
            if os.path.exists(status_path):
                dataset_info['multiple_event_status_df'] = pd.read_csv(status_path)
        
        # Determine photo root
        dataset_info['photo_root'] = determine_photo_root(directory)
        
        dataset_info['is_loaded'] = True
        
        return {'success': True}
    
    except Exception as e:
        return {'success': False, 'message': f'Error loading dataset: {str(e)}'}

@app.route('/main')
def main_interface():
    """Main interface showing species list"""
    if not dataset_info['is_loaded']:
        return redirect(url_for('index'))
    
    # Get species with event counts
    species_with_counts = get_species_with_counts()
    
    return render_template('main_interface.html', 
                         species_list=species_with_counts,
                         dataset_info=get_dataset_summary())

def get_species_with_counts():
    """Get list of species with their event counts"""
    if dataset_info['event_tagging_df'] is None:
        return []
    
    # Count events per species
    species_counts = dataset_info['event_tagging_df']['speciesID'].value_counts().to_dict()
    
    # Merge with species names
    species_list = []
    for species_id, count in species_counts.items():
        species_row = dataset_info['species_df'][dataset_info['species_df']['id'] == species_id]
        if not species_row.empty:
            species_info = {
                'id': species_id,
                'common_name': species_row.iloc[0]['Common Name'],
                'species_name': species_row.iloc[0]['Species Name'],
                'event_count': count
            }
            species_list.append(species_info)
    
    return sorted(species_list, key=lambda x: x['event_count'], reverse=True)

def get_dataset_summary():
    """Get summary information about the dataset"""
    summary = {}
    
    if dataset_info['metadata_df'] is not None and not dataset_info['metadata_df'].empty:
        summary.update(dataset_info['metadata_df'].iloc[0].to_dict())
    
    summary['total_species'] = len(dataset_info['species_df']) if dataset_info['species_df'] is not None else 0
    summary['total_stations'] = len(dataset_info['stations_df']) if dataset_info['stations_df'] is not None else 0
    summary['total_sequences'] = len(os.listdir(os.path.join(dataset_info['root_path'], 'sequences'))) if dataset_info['root_path'] else 0
    
    return summary

@app.route('/species/<species_id>')
def species_sequences(species_id):
    """Show sequences for a specific species"""
    if not dataset_info['is_loaded']:
        return redirect(url_for('index'))
    
    # Convert species_id to int, handling both int and float strings
    try:
        species_id = int(float(species_id))
    except (ValueError, TypeError):
        return "Invalid species ID", 400
    
    # Get species information
    species_row = dataset_info['species_df'][dataset_info['species_df']['id'] == species_id]
    if species_row.empty:
        return "Species not found", 404
    
    species_info = species_row.iloc[0].to_dict()
    
    # Get sequences for this species
    sequences = get_sequences_for_species(species_id)
    
    return render_template('species_sequences.html', 
                         species=species_info,
                         sequences=sequences)

def get_sequences_for_species(species_id):
    """Get all sequences that contain a specific species"""
    if dataset_info['event_tagging_df'] is None:
        return []
    
    # Get events for this species
    species_events = dataset_info['event_tagging_df'][
        dataset_info['event_tagging_df']['speciesID'] == species_id
    ]
    
    sequences = []
    for _, event in species_events.iterrows():
        ctid = event['ctid']
        interval = event['event']
        
        # Get status if available
        status = 'unknown'
        if dataset_info['multiple_event_status_df'] is not None:
            status_row = dataset_info['multiple_event_status_df'][
                (dataset_info['multiple_event_status_df']['ctid'] == ctid) &
                (dataset_info['multiple_event_status_df']['interval'] == interval)
            ]
            if not status_row.empty:
                status = status_row.iloc[0]['status']
        
        # Handle NaN values properly
        individual_name = event.get('indName', '')
        if pd.isna(individual_name) or str(individual_name).lower() == 'nan':
            individual_name = ''
        
        sequence_info = {
            'ctid': ctid,
            'interval': interval,
            'status': status,
            'num_individuals': event['numInd'],
            'individual_name': individual_name,
            'sex': event.get('Sex', 'Unknown'),
            'age': event.get('Age', 'Unknown')
        }
        sequences.append(sequence_info)
    
    return sequences

@app.route('/sequence/<ctid>/<int:interval>')
def view_sequence(ctid, interval):
    """View a specific sequence with GIF and tagging information"""
    if not dataset_info['is_loaded']:
        return redirect(url_for('index'))
    
    # URL decode the ctid
    ctid = unquote(ctid)
    
    # The folder name might use dots instead of spaces
    # Try both the original CTID and a dot-replaced version
    potential_folder_names = [
        ctid,
        ctid.replace(' ', '.'),
        ctid.replace(' ', '_'),  # Some systems might use underscores
    ]
    
    gif_filename = f'sequence.{interval}.gif'
    gif_path = None
    
    # Try to find the correct folder
    sequences_dir = os.path.join(dataset_info['root_path'], 'sequences')
    for folder_name in potential_folder_names:
        test_path = os.path.join(sequences_dir, folder_name, gif_filename)
        if os.path.exists(test_path):
            gif_path = test_path
            break
    
    if not gif_path:
        # List available directories for debugging
        available_dirs = []
        if os.path.exists(sequences_dir):
            available_dirs = [d for d in os.listdir(sequences_dir) if os.path.isdir(os.path.join(sequences_dir, d))]
        
        error_msg = f"Sequence not found. Looking for CTID: {ctid}, tried folder names: {potential_folder_names}"
        if available_dirs:
            error_msg += f". Available directories: {available_dirs[:10]}"  # Show first 10 for debugging
        
        return error_msg, 404
    
    # Get all species tagged in this sequence
    sequence_tags = get_sequence_tags(ctid, interval)
    
    # Get photos in this sequence for photo view
    photos = get_sequence_photos(ctid, interval)
    
    # Extract the actual folder name that worked
    actual_folder_name = os.path.basename(os.path.dirname(gif_path))
    
    return render_template('sequence_viewer.html',
                         ctid=ctid,
                         interval=interval,
                         gif_filename=gif_filename,
                         actual_folder_name=actual_folder_name,
                         sequence_tags=sequence_tags,
                         photos=photos)

def get_sequence_tags(ctid, interval):
    """Get all species tagged in a sequence"""
    if dataset_info['event_tagging_df'] is None:
        return []
    
    # Get all tags for this sequence
    tags = dataset_info['event_tagging_df'][
        (dataset_info['event_tagging_df']['ctid'] == ctid) &
        (dataset_info['event_tagging_df']['event'] == interval)
    ]
    
    sequence_tags = []
    for _, tag in tags.iterrows():
        species_row = dataset_info['species_df'][
            dataset_info['species_df']['id'] == tag['speciesID']
        ]
        
        if not species_row.empty:
            species_info = species_row.iloc[0]
            
            # Handle NaN values properly
            individual_name = tag.get('indName', '')
            if pd.isna(individual_name) or str(individual_name).lower() == 'nan':
                individual_name = ''
            
            tag_info = {
                'species_id': tag['speciesID'],
                'common_name': species_info['Common Name'],
                'species_name': species_info['Species Name'],
                'num_individuals': tag['numInd'],
                'individual_name': individual_name,
                'sex': tag.get('Sex', 'Unknown'),
                'age': tag.get('Age', 'Unknown')
            }
            sequence_tags.append(tag_info)
    
    return sequence_tags

def get_sequence_photos(ctid, interval):
    """Get individual photos in a sequence"""
    if dataset_info['intervals_df'] is None:
        return []
    
    # Get photos for this sequence
    photos = dataset_info['intervals_df'][
        (dataset_info['intervals_df']['ctid'] == ctid) &
        (dataset_info['intervals_df']['interval'] == interval)
    ]
    
    photo_list = []
    for _, photo in photos.iterrows():
        photo_path = photo['fn']
        
        # Get species tags for this specific photo
        photo_tags = []
        if dataset_info['multiple_event_tags_df'] is not None:
            # Convert absolute path to relative path for matching
            relative_path = photo_path
            if dataset_info['photo_root']:
                relative_path = photo_path.replace(dataset_info['photo_root'], '').lstrip('/')
            
            tags = dataset_info['multiple_event_tags_df'][
                dataset_info['multiple_event_tags_df']['fn'] == f'/{relative_path}'
            ]
            
            for _, tag in tags.iterrows():
                species_row = dataset_info['species_df'][
                    dataset_info['species_df']['id'] == tag['species']
                ]
                if not species_row.empty:
                    species_info = species_row.iloc[0]
                    photo_tags.append({
                        'species_id': tag['species'],
                        'common_name': species_info['Common Name'],
                        'species_name': species_info['Species Name']
                    })
        
        photo_info = {
            'path': photo_path,
            'datetime': photo['dt'],
            'location': photo['location'],
            'tags': photo_tags
        }
        photo_list.append(photo_info)
    
    return sorted(photo_list, key=lambda x: x['datetime'])

@app.route('/serve_gif/<ctid>/<filename>')
def serve_gif(ctid, filename):
    """Serve GIF files from sequences directory"""
    # URL decode the ctid
    ctid = unquote(ctid)
    
    # Try different folder name formats
    potential_folder_names = [
        ctid,
        ctid.replace(' ', '.'),
        ctid.replace(' ', '_'),
    ]
    
    sequences_dir = os.path.join(dataset_info['root_path'], 'sequences')
    
    for folder_name in potential_folder_names:
        gif_path = os.path.join(sequences_dir, folder_name, filename)
        if os.path.exists(gif_path):
            # Security: ensure the path is within the dataset
            full_path = os.path.abspath(gif_path)
            dataset_root = os.path.abspath(dataset_info['root_path'])
            
            if not full_path.startswith(dataset_root):
                return "Access denied", 403
            
            return send_file(full_path)
    
    return "GIF not found", 404

@app.route('/serve_image/<path:image_path>')
def serve_image(image_path):
    """Serve images from the dataset"""
    # Handle both absolute and relative paths
    if os.path.isabs(image_path):
        full_path = image_path
    else:
        # If relative, assume it's relative to dataset root or photo root
        if dataset_info['photo_root']:
            full_path = os.path.join(dataset_info['photo_root'], image_path)
        else:
            full_path = os.path.join(dataset_info['root_path'], image_path)
    
    # Security: ensure the path is within the dataset
    full_path = os.path.abspath(full_path)
    dataset_root = os.path.abspath(dataset_info['root_path'])
    
    if not full_path.startswith(dataset_root):
        return "Access denied", 403
    
    if not os.path.exists(full_path):
        return "Image not found", 404
    
    return send_file(full_path)

@app.route('/api/photo_root', methods=['POST'])
def update_photo_root():
    """Update the photo root path"""
    new_root = request.json.get('photo_root')
    if new_root and os.path.exists(new_root):
        dataset_info['photo_root'] = new_root
        return jsonify({'success': True})
    return jsonify({'success': False, 'message': 'Invalid path'})

if __name__ == '__main__':
    app.run(debug=True, host='127.0.0.1', port=5000)
