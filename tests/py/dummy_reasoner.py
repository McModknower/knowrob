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
		# create a query that checks if subj and obj both love the same person
		query_term = GraphSequence([
			GraphPattern(subj, self.loves, Variable("z")),
			GraphPattern(obj, self.loves, Variable("z"))])
		# execute the query using the storage of the reasoner and call handleSolution for each solution.
		# a solution is represented as a dictionary of variable bindings that can be applied to the
		# predicate to create a new instance.
		self.storage().query(GraphQuery(query_term), lambda bindings: self.handleSolution(query, predicate, bindings))
		return True

	def handleSolution(self, query, predicate, bindings):
		# create a new instance of the predicate with the bindings
		instance = applyBindings(predicate, bindings)
		# check if the subject and object are different
		subj = instance.arguments()[0]
		obj = instance.arguments()[2]
		if subj == obj:
			# skip self, cannot be done in graph query at the moment as no NOT_EQUAL
			# operator is part of the query language.
			return
		# generate a new answer with the bindings, and add the instance as grounding
		query.push(bindings)
