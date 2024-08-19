from knowrob import *

class DummyReasoner(GoalDrivenReasoner):
	def __init__(self):
		super(DummyReasoner, self).__init__()
		#enableFeature(GoalDrivenReasonerFeature.SupportsSimpleConjunctions)
		#enableFeature(GoalDrivenReasonerFeature.SupportsExtensionalGrounding)
		self.defineRelation(PredicateIndicator("dummy", 2))
		print("DummyReasoner()")

	def setDataBackend(self, dataBackend):
		print("setDataBackend: " + str(type(dataBackend)))
		return None

	def initializeReasoner(self, config):
		print("initializeReasoner: " + str(type(config)))
		return True

	def evaluateQuery(self, query):
		formula = query.formula()
		literal = formula.literals()[0]
		predicate = literal.predicate()
		print("evaluateQuery: " + str(predicate))
		return None
