try:
	# This case works in ros1 environments
	from knowrob.kb import *
except ImportError:
	# If the import fails, import the knowrob.so directly
	from knowrob import *


def atom_to_python(term):
	# make some tests that the class hierarchy is correct
	# NOTE: unfortunately, checking the type is not enough. There are runtime conversions done
	#       that could fail. So it makes sense to test the mapping more thoroughly for different mapped types.
	assert isinstance(term, Term), "argument is not a Term"
	assert isinstance(term, Atom), "argument is not an Atom"
	# check for the existence of some methods
	assert hasattr(term, "stringForm"), "term has no stringForm method"
	assert hasattr(term, "variables"), "term has no variables method"
	assert hasattr(term, "atomType"), "term has no atomType method"
	assert hasattr(term, "atomicType"), "term has no atomicType method"
	assert hasattr(term, "termType"), "term has no termType method"
	assert hasattr(term, "isAtomic"), "term has no isAtomic method"
	# check that some boolean methods return the right value
	assert term.isAtomic(), "term is not atomic"
	assert term.isAtom(), "term is not an atom"
	assert not term.isVariable(), "term is a variable"
	assert not term.isFunction(), "term is a function"
	assert not term.isNumeric(), "term is numeric"
	assert not term.isString(), "term is a string"
	# check that some enums can be accessed
	assert type(term.atomType()) == AtomType, "atomType is not an AtomType"
	assert term.atomType() == AtomType.REGULAR, "atomType is not REGULAR"
	assert type(term.atomicType()) == AtomicType, "atomicType is not an AtomicType"
	assert term.atomicType() == AtomicType.ATOM, "atomicType is not ATOM"
	assert type(term.termType()) == TermType, "termType is not a TermType"
	assert term.termType() == TermType.ATOMIC, "termType is not ATOMIC"
	# test that string view can be mapped to Python string
	assert type(term.stringForm()) == str, "stringForm is not a string"


def modify_triple_in_python(triple):
	assert isinstance(triple, FramedTriple), "argument is not a FramedTriple"
	assert hasattr(triple, "setSubject"), "term has no setSubject method"
	assert hasattr(triple, "setPredicate"), "term has no setPredicate method"
	triple.setSubject("olleh")
	triple.setPredicate("swonk")


def string_copy_from_python():
	# test that string can be copied from Python to C++.
	# here the constructor actually takes a string_view argument which is then copied internally.
	term = String("hello")
	assert term is not None, "term is None"
	assert term.stringForm() == "hello", "stringForm is not 'hello'"
	return term


def optional_is_none(optional):
	assert optional is None, "optional is not None"


def optional_is_not_none(optional):
	assert optional is not None, "optional is None"


def set_xsd_optional(optional):
	assert optional == XSDType.STRING, "optional is not STRING"
	optional = XSDType.DOUBLE
	assert optional == XSDType.DOUBLE, "optional is not DOUBLE"


def query_knowledge_base(optional):
	# Initialize the knowledge base
	InitKnowledgeBase()
	# Load the settings
	kb = KnowledgeBase(InterfaceUtils.loadSettings())
	# Create a formula for the query
	phi = Formula(QueryParser::parse("test:hasAncestor(X, Y)"))
	# Apply the modality
	mPhi = InterfaceUtils::applyModality(Modality::POSS, phi)
	# Get Result Stream
	resultStream = kb.submitQuery(mPhi)
	resultQueue = resultStream.createQueue()
	# Get the result
	nextResult = resultQueue.pop_front()
	# Check if the result is an posititve answer
	assert nextResult.tokenType() == TokenType.ANSWER_TOKEN
	# Get the answer
	answer = (Answer) nextResult
	assert answer->isPositive()
	positiveAnswer = (AnswerYes) answer
	# Check if the substitution is not empty
	assert not positiveAnswer.substitution().empty()
	# Get result
	for (pair in  positiveAnswer.substitution()):
		term = pair.second.second
		assert term.termType() == TermType.ATOMIC)
		atomic = (Atomic) term
		assert atomic.atomicType() == AtomicType.ATOM or atomic.atomicType() == AtomicType.STRING
		stringResult = atomic.stringForm().data()
		assert stringResult == "test:Lea"

