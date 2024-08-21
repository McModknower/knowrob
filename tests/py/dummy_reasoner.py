from knowrob import *


class DummyReasoner(GoalDrivenReasoner):
	def __init__(self):
		super(DummyReasoner, self).__init__()
		self.loves = IRIAtom("http://knowrob.org/kb/lpn#loves")
		# The reasoner defines a new predicate lpn:jealous that can be evaluated by the reasoner
		self.define(IRIAtom("http://knowrob.org/kb/lpn#jealous"))

	def initializeReasoner(self, config):
		# nothing to do here
		return True

	def evaluateQuery(self, query):
		literal = query.formula().literals()[0]
		predicate = literal.predicate()
		subj = predicate.arguments()[0]
		obj = predicate.arguments()[2]
		# Create a query that checks if subj and obj both love the same person.
		# A builtin is used to ensure that subj and obj are different.
		query_term = GraphSequence([
			GraphPattern(subj, self.loves, Variable("z")),
			GraphPattern(obj, self.loves, Variable("z")),
			GraphBuiltin.notEqual(subj, obj)])
		# execute the query using the storage of the reasoner and call query.push for each solution.
		# a solution is represented as a dictionary of variable bindings that can be applied to the
		# query formula to replace variables with constants.
		self.storage().query(GraphQuery(query_term), query.push)
		return True
