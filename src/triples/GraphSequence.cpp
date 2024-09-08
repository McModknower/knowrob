/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/triples/GraphSequence.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

void GraphSequence::write(std::ostream &os) const {
	os << "(";
	for (std::size_t i = 0; i < terms_.size(); i++) {
		if (i > 0) {
			os << ", ";
		}
		os << *terms_[i];
	}
	os << ")";
}

namespace knowrob::py {
	template<>
	void createType<GraphSequence>() {
		using namespace boost::python;

		class_<GraphSequence, bases<GraphTerm>, std::shared_ptr<GraphSequence>, boost::noncopyable>
		        ("GraphSequence", init<>())
		        .def(init<const std::vector<std::shared_ptr<GraphTerm>> &>());
	}
}
