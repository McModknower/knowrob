/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/semweb/RDFIndicator.h>

using namespace knowrob;

RDFIndicator::RDFIndicator(const PredicatePtr &predicate) {

	// there is the case of property variables, and representation using triple/3 predicate
	// which needs special handling here.
	static const AtomPtr tripleFunctor = Atom::Tabled("triple");
	if (predicate->arity() == 3 && *predicate->functor() == *tripleFunctor) {
		auto predicateTerm = predicate->arguments()[1];
		arity = 2;
		if (predicateTerm->termType() == TermType::ATOMIC) {
			functor = std::static_pointer_cast<Atomic>(predicateTerm)->stringForm();
		}
	} else {
		arity = predicate->arity();
		functor = predicate->functor()->stringForm();
	}
}

RDFIndicator::RDFIndicator(size_t arity) : arity(arity) {}

RDFIndicator::RDFIndicator(std::string_view functor, size_t arity) : functor(functor), arity(arity) {}
