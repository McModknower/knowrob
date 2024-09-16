Python Integration {#python}
============

KnowRob integrates with the Python language through the boost::python library.
The way it works is that the C++ classes are wrapped in Python classes
using the `boost::python` library.
The build process then creates a module file called `knowrob.so` that can be imported
in Python using e.g. `import knowrob`.

There are two main intended use cases:
1. Python applications that internally manage a `KnowledgeBase` object.
2. KnowRob plugins for `Storage` and `Reasoner` that are written in Python: 
  you can write a Python class that inherits from `Storage` or `Reasoner`
  and implement the virtual methods. For more details on how to write a Python plugin,
  see the [README](../../reasoner/README.md) in the reasoner directory.

Currently, only the C++ classes which are needed for these use-cases are available
in Python, i.e. the classes themselves and the types that appear in their public
interfaces.
Please further note that the integration is at an *early stage*, and some important
features might still be missing. If you encounter one, please report it via the issue tracker.

### Python Reasoner Plugin

To write a Python plugin for the reasoner, you need to create a Python class
that inherits from `GoalDrivenReasoner` or `DataDrivenReasoner` or one of its
subclasses, and implement the virtual methods.
An example of a Python reasoner plugin can be found in the `tests` directory.
A template for a reasoner that defined RDF relations is like this:

```python
from knowrob import *

class MyReasonerType(RDFGoalReasoner):
	def __init__(self):
		super(DummyReasoner, self).__init__()
		# The reasoner defines a new predicate that can be evaluated by the reasoner
		self.defineRelation(IRIAtom("http://my.org/jealous"))

	def initializeReasoner(self, config: PropertyTree) -> bool:
		# Read reasoner specific settings from the configuration
		return True

	def evaluate(self, goal: RDFGoal) -> bool:
		# The goal is a conjunction that contains a single RDF literal of the form jealous(s, o).
		# There will not be any other literals in the goal as we have not enabled
		# the use of complex formulas in the reasoner configuration (via enableFeature/1).
		return True
```

Please refer to the C [API Documentation](https://knowrob.github.io/knowrob/) for more information on
the available interfaces.

It can be loaded into KnowRob by adding a reasoner configuration to the
settings file, e.g.:

```json
    {
      "name": "my_reasoner",
      "type": "MyReasonerType",
      "module": "/path/to/my_reasoner.py",
      "data-backend": "pl"
    }
```

This assumes that the Python module "my_reasoner.py" contains a class
"MyReasonerType" that inherits from `GoalDrivenReasoner` or `DataDrivenReasoner`.
It further connects to the Prolog storage backend "pl" which must be defined
in the settings file as well.

### Limitations

There are a couple of specifics that require special attention.

One of them is that there are problems when Python classes implement
multiple C++ interfaces. This could be useful to define a `Reasoner` which
is also a `Storage`, but it is not supported at the moment.

Another limitation is that when overwriting a virtual method in Python, it seems
not possible to explicitly call the base class method.
However, as a workaround for most cases a wrapper class can be defined on the
C++ side that also calls the base class method.

Finally, the Python code is currently missing doc strings.
There is unfortunately no way to automatically generate them using
the `boost::python` library.
However, at a later point we could generate Python doc strings from the existing API
documentation using a custom target in the CMake build system.

