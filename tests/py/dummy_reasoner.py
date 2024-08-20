from knowrob import *

class DummyReasoner(GoalDrivenReasoner):
	def __init__(self):
		super(DummyReasoner, self).__init__()
		#enableFeature(GoalDrivenReasonerFeature.SupportsSimpleConjunctions)
		#enableFeature(GoalDrivenReasonerFeature.SupportsExtensionalGrounding)
		self.defineRelation(PredicateIndicator("jealous", 2))
		self.storage = None

	def setDataBackend(self, storage):
		# keep a reference on the storage, as it is used in evaluateQuery
		self.storage = storage

	def initializeReasoner(self, config):
		# nothing to do here
		return True

	def evaluateQuery(self, query):
		formula = query.formula()
		literal = formula.literals()[0]
		predicate = literal.predicate()
		loves = 'http://knowrob.org/kb/lpn#loves'
		x = predicate.arguments()[0]
		y = predicate.arguments()[2]
		z = Variable("z")
		p1 = Predicate(loves, [x, z])
		p2 = Predicate(loves, [y, z])
		print("evaluateQuery: " + str(predicate) + " <-- " + str([p1, p2]))
		gq = GraphSequence([GraphPattern(p1), GraphPattern(p2)])
		return None
