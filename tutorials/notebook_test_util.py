import nbformat
import os
import shutil
import sys
import argparse 

def check_notebooks_for_expected_output(notebooks_dir):
    """Check that all notebooks with 'outputs' have 'expected_output' in metadata and that 'outputs' is empty."""
    all_valid = True
    notebooks = [os.path.join(notebooks_dir, nb) for nb in os.listdir(notebooks_dir) if nb.endswith('.ipynb')]
    for notebook_path in notebooks:
        with open(notebook_path, 'r', encoding='utf-8') as f:
            notebook = nbformat.read(f, as_version=4)
        for i, cell in enumerate(notebook.cells):
            if cell.cell_type == 'code' and 'outputs' in cell:
                # Check if 'expected_output' is in the metadata
                if 'expected_output' not in cell['metadata']:
                    print(f"Error: Cell {i} in notebook {notebook_path} has 'outputs' but no 'expected_output' in metadata.")
                    all_valid = False
                # Check if 'outputs' is empty
                if cell['outputs'] != []:
                    print(f"Error: Cell {i} in notebook {notebook_path} has non-empty 'outputs'.")
                    all_valid = False
        if all_valid:
            print(f"Notebook {notebook_path} formatted correctly.")

    return all_valid

def move_output_to_metadata(notebook_path):
    """Move outputs to 'metadata.expected_output' and ensure 'outputs' is empty."""
    with open(notebook_path, 'r', encoding='utf-8') as f:
        notebook = nbformat.read(f, as_version=4)
    
    modified = False
    # Iterate through the notebook cells
    for cell in notebook.cells:
        if cell.cell_type == 'code' and 'outputs' in cell:
            # Store outputs in the metadata
            cell['metadata']['expected_output'] = cell['outputs']
            cell['outputs'] = []  # Clear the regular outputs
            modified = True
    
    if modified:
        # Save the notebook with the modified structure
        with open(notebook_path, 'w', encoding='utf-8') as f:
            nbformat.write(notebook, f)
        print(f"Modified notebook: {notebook_path}")

def restore_output_from_metadata(notebook_path, temp_dir):
    """Restore outputs from 'metadata.expected_output'."""
    # Create a temporary copy of the notebook for testing
    notebook_name = os.path.basename(notebook_path)
    temp_notebook_path = os.path.join(temp_dir, notebook_name)
    shutil.copy(notebook_path, temp_notebook_path)
    
    with open(temp_notebook_path, 'r', encoding='utf-8') as f:
        notebook = nbformat.read(f, as_version=4)
    
    modified = False
    # Iterate through the notebook cells
    for cell in notebook.cells:
        if cell.cell_type == 'code' and 'metadata' in cell and 'expected_output' in cell['metadata']:
            cell['outputs'] = cell['metadata']['expected_output']  # Restore the outputs
            modified = True
    
    if modified:
        # Save the modified notebook in the temp directory
        with open(temp_notebook_path, 'w', encoding='utf-8') as f:
            nbformat.write(notebook, f)
        print(f"Prepared notebook for testing: {temp_notebook_path}")
    
    return temp_notebook_path

def process_notebook(notebook, action, temp_dir=None):
    """Process a single notebook based on the action ('store' or 'restore')."""
    if action == "store":
        move_output_to_metadata(notebook)
    elif action == "restore":
        if temp_dir:
            restore_output_from_metadata(notebook, temp_dir)
        else:
            print("Error: Temporary directory is required for 'restore' action.")

def process_notebooks(notebooks_dir, action, temp_dir=None):
    """Process all notebooks in a directory based on the action."""
    notebooks = [os.path.join(notebooks_dir, nb) for nb in os.listdir(notebooks_dir) if nb.endswith('.ipynb')]
    
    for notebook in notebooks:
        process_notebook(notebook, action, temp_dir)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Process Jupyter notebooks by moving outputs to/from metadata and checking for consistency.')
    parser.add_argument('action', choices=['store', 'restore', 'check'], help="Action to perform: 'store' moves output to metadata, 'restore' prepares for testing, 'check' ensures 'expected_output' is present and 'outputs' is empty.")
    parser.add_argument('--notebooks-dir', help='Directory containing the Jupyter notebooks.')
    parser.add_argument('--notebook', help='Specific notebook to process.')
    parser.add_argument('--temp-dir', help='Temporary directory for storing the restored notebooks for testing (only required for "restore" action).')
    
    args = parser.parse_args()
    
    if args.action == "check":
        # Perform the check that all notebooks have 'expected_output' if they have 'outputs' and that 'outputs' is empty
        if args.notebooks_dir:
            # Check all notebooks in the directory
            if not check_notebooks_for_expected_output(args.notebooks_dir):
                sys.exit(1)
        else:
            print("check only works on notebooks-dir")
            sys.exit(1)
    else:
        # Process storing or restoring for notebooks
        if args.notebook:
            # Process a single notebook
            process_notebook(args.notebook, args.action, args.temp_dir)
        elif args.notebooks_dir:
            # Process all notebooks in the directory
            process_notebooks(args.notebooks_dir, args.action, args.temp_dir)
        else:
            print("Error: You must specify either --notebook or --notebooks-dir.")
            sys.exit(1)
