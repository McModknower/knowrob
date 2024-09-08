/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/triples/GraphUnion.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

void GraphUnion::write(std::ostream &os) const {
	os << "Union(";
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
	void createType<GraphUnion>() {
		using namespace boost::python;

		class_<GraphUnion, bases<GraphTerm>, std::shared_ptr<GraphUnion>, boost::noncopyable>
		        ("GraphUnion", init<>())
		        .def(init<const std::vector<std::shared_ptr<GraphTerm>> &>());
	}
}
