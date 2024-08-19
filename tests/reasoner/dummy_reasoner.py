from knowrob import *

class DummyReasoner(GoalDrivenReasoner):
	def __init__(self):
		super(DummyReasoner, self).__init__()
		#enableFeature(GoalDrivenReasonerFeature.SupportsSimpleConjunctions)
		#enableFeature(GoalDrivenReasonerFeature.SupportsExtensionalGrounding)
		self.defineRelation(PredicateIndicator("dummy", 1))

	def setDataBackend(self, dataBackend):
		print("setDataBackend: " + str(dataBackend))
		return None

	def initializeReasoner(self, config):
		print("initializeReasoner: " + str(config))
		return True

	def evaluateQuery(self, query):
		print("evaluateQuery: " + str(query))
		return None
