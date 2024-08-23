\page reasoner Reasoner

A reasoner is a component that can infer new knowledge from existing knowledge.
In KnowRob, the actual mechanisms to perform the reasoning are configurable and can be exchanged.
KnowRob defines abstract interfaces for reasoning components, which can be implemented
through plugins.
On the top level, the interface for reasoner plugins distinguishes between data-driven and
goal-driven reasoning (see below).
Plugins can be written in C++ in form of a shared library with a common access point
or in Python.
A few built-in reasoners are also available including one for rule-based reasoning
using the Prolog language.

### Data-driven Reasoning

Data-driven reasoning is based on the idea that new knowledge can be inferred from existing
knowledge by applying rules or algorithms to the data.
In KnowRob, data-driven reasoning is implemented through reasoner plugins that can be
configured to perform reasoning tasks.
The base class for data-driven reasoners is `DataDrivenReasoner`.
It defines interfaces for starting, stopping and updating the reasoner.
In addition, data driven reasoner may generate events to notify the knowledge base
about their state.
Depending on supported features, data-driven reasoners may be automatically updated
at a fixed rate, or when they generate an invalidation event.
Some reasoner may further choose to update themselves, maybe in case they can make
use of an internal event system.

The event mechanism is also used to notify the knowledge base about new knowledge
that has been inferred.
This is done by generating a `Assertion`, `Retraction` or `Replacement` event:

- `Assertion`: additional inferences that should be added to existing ones.
- `Retraction`: previously inferred knowledge that should be removed.
- `Replacement`: replace existing inferences with new ones.

### Goal-driven Reasoning

Goal-driven reasoning is based on the idea that new knowledge can be inferred from existing
knowledge in a goal-oriented way by only considering the knowledge that is relevant to
a specific goal.
In KnowRob, goal-driven reasoning is implemented through reasoner plugins that can be
configured to perform reasoning tasks.
The base class for goal-driven reasoners is `GoalDrivenReasoner`.
Goal-driven reasoner must explicitly state which relations they define.
The knowledge base will only consult a given goal-driven reasoner if the goal refers
to a relation that the reasoner defines.
The interface further includes a method to submit a query to the reasoner.

### Reasoner Plugins

Reasoner plugins can be implemented in C++ or Python.

#### C++ Reasoner Plugins

A C++ reasoner plugin can be implemented as follows:

- Create a new class that inherits from `DataDrivenReasoner` or `GoalDrivenReasoner`.
- Call the `REASONER_PLUGIN` macro to create an entry point for KnowRob to load the plugin.
- Compile the code as a shared library (`.so` file under Linux).
- Refer to the library in a settings file within a "reasoner" block, using the "library" key.

#### Python Reasoner Plugins

A Python reasoner plugin can be implemented as follows:

- Create a new class that inherits from `DataDrivenReasoner`, `GoalDrivenReasoner` or `RDFGoalReasoner`.
- Refer to the class in a settings file within a "reasoner" block, using the "module" key.

#### Example

The following example shows how to implement a simple reasoner plugin in Python:

```python
from knowrob import *


class DummyReasoner(RDFGoalReasoner):
	def __init__(self):
		super(DummyReasoner, self).__init__()
		self.loves = IRIAtom("http://knowrob.org/kb/lpn#loves")
		# The reasoner defines a new predicate lpn:jealous that can be evaluated by the reasoner
		self.define(IRIAtom("http://knowrob.org/kb/lpn#jealous"))

	def initializeReasoner(self, config: PropertyTree) -> bool:
		# nothing to do here
		return True

	def evaluate(self, goal: RDFGoal) -> bool:
		# The goal is a conjunction that contains a single RDF literal of the form jealous(s, o).
		# There will not be any other literals in the goal as we have not enabled
		# the use of complex formulas in the reasoner configuration (via enableFeature/1).
		literal = goal.rdfLiterals()[0]
		s : Term = literal.subjectTerm()
		o : Term = literal.objectTerm()
		# Push a debug message to the logger of the knowledge base.
		logDebug("Checking if %s is jealous of %s" % (s, o))
		# Create a query that checks if subj and obj both love the same person.
		# A builtin is used to ensure that subj and obj are different.
		query_term = GraphSequence([
			GraphPattern(s, self.loves, Variable("z")),
			GraphPattern(o, self.loves, Variable("z")),
			GraphBuiltin.notEqual(s, o)])
		# Execute the query using the storage of the reasoner and call query.push for each solution.
		# A solution is represented as a dictionary of variable bindings that can be applied to the
		# query formula to replace variables with constants.
		self.storage().query(GraphQuery(query_term), goal.push)
		return True
```

### Available Reasoning Components

The following reasoner are available in KnowRob:

- \subpage prolog
- \subpage mongolog
- \subpage owl
- \subpage esg
- \subpage srdl
- \subpage swrl
