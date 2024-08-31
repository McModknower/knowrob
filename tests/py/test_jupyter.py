import json
import gc
from knowrob import *


def test_code_cell(cell):
	#execution_count = cell["execution_count"]
	source = "".join(cell["source"])
	try:
		exec(source, globals())
	except Exception as e:
		logError("Error executing code cell: " + source)
		raise e


def test_notebook(notebook_file):
	# Load json data of notebook and attempt to execute the code cells
	with open(notebook_file) as f:
		nb = json.load(f)
		gc.disable()
		for cell in nb["cells"]:
			if cell["cell_type"] == "code":
				test_code_cell(cell)
		gc.enable()
