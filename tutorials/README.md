# KnowRob Jupyter Tutorials

This directory contains Jupyter notebooks designed to teach users how to work with KnowRob. You can run these tutorials locally on your machine, using Docker, or remotely on Binder. This README provides instructions on each method and explains how to add your own tutorials to the repository.

## Running Tutorials

### 1. Running Locally with `jupyter notebook`

You can run the Jupyter tutorials locally by following these steps:

#### Prerequisites:
- Add KnowRob to your Python environment by setting the `PYTHONPATH` to the `build` directory where KnowRob is located.

#### Steps:

1. Navigate to the `tutorials` directory:
2. Start the Jupyter notebook server:
   ```bash
   jupyter notebook
   ```
3. Open one of the tutorial notebooks (e.g., `intro.ipynb`) from your browser, and begin running the cells.

### 2. Running Locally with Docker

Alternatively, you can run the Jupyter notebooks locally using Docker, which provides a controlled environment with all dependencies set up for you.

#### Steps:
1. Ensure you have Docker installed on your machine.
2. Navigate to the root of your KnowRob repository in your terminal.
3. Run the following commands to build and start the Docker containers:
   ```bash
   chmod -R g+w ./ && export GID=$(id -g) && \
   docker compose -f ./binder/docker-compose.yml up --build
   ```
4. Open your web browser and navigate to the provided URL to start working with the Jupyter notebooks.

### 3. Running Remotely on Binder

You can run the Jupyter notebooks remotely using Binder without having to install anything locally. Binder provides an online environment where you can directly interact with the notebooks.

#### Steps:
1. Open your web browser and navigate to the following Binder link:
   [Run on Binder](https://binder.intel4coro.de/v2/gh/knowrob/knowrob.git/dev?labpath=jupyter%2Fintro.ipynb)
2. This will launch a remote instance of Jupyter Lab, where you can open and run the tutorial notebooks.

## Adding Your Own Tutorials

If you wish to add your own tutorials to the KnowRob project, follow these steps:

### 1. Add a New Jupyter Notebook
- Place your new Jupyter notebook in the `tutorials` directory.
- Name your notebook appropriately to reflect its contents and topic.

### 2. Run the Notebook and Save with Outputs
- Open and run the notebook locally.
- After running all the cells, save the notebook with the outputs by clicking **File > Save and Checkpoint**. Ensure that the outputs are stored so that they can be used in the testing pipeline.

### 3. Store the Outputs for Unit Tests
To make sure your tutorial is testable in the CI pipeline, use the provided script to store the notebook's outputs in its metadata. This is required to set up the notebook for unit testing with `nbval`.

#### Steps:
1. Run the script to store the outputs for testing:
   ```bash
   python3 notebook_test_util.py store --notebook ./jupyter/tutorials/<your-notebook>.ipynb
   ```
   This script will move the outputs of the cells into the `metadata` field of the notebook.
   
You can now commit and push your changes to the repository, including the notebook with the stored outputs.