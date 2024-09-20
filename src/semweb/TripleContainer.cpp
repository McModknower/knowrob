/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/semweb/TripleContainer.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

ProxyTripleContainer::ProxyTripleContainer(const std::vector<TriplePtr> *triples)
		: triples_(triples) {
}

ProxyTripleContainer::ProxyTripleContainer(const std::vector<TriplePtr> &triples)
		: triples_(&triplesData_), triplesData_(triples) {
	// take the ownership of the triples
	for (uint32_t i = 0; i < triples.size(); ++i) {
		if (triples[i].owned) {
			triplesData_[i].owned = true;
			triples[i].owned = false;
		}
	}
}

TripleContainer::ConstGenerator ProxyTripleContainer::cgenerator() const {
	return [it = triples_->begin(), end = triples_->end()]()
			mutable -> const TriplePtr * {
		if (it == end) {
			return nullptr;
		}
		auto &ptr = *it;
		++it;
		return &ptr;
	};
}

TripleViewBatch::TripleViewBatch(uint32_t batchSize)
		: data_(batchSize), batchSize_(batchSize), actualSize_(0) {
}

void TripleViewBatch::add(const TriplePtr &triple) {
	if (actualSize_ < batchSize_) {
		auto &entry = data_[actualSize_++];
		if (entry.owned && entry.ptr) {
			delete entry.ptr;
		}
		if (triple.owned) {
			entry.ptr = triple.ptr;
			entry.owned = true;
			triple.owned = false;
		} else {
			entry.ptr = new TripleCopy(*triple.ptr);
			entry.owned = true;
		}
	}
}

TripleContainer::ConstGenerator TripleViewBatch::cgenerator() const {
	return [this, i = std::size_t(0)]() mutable -> const TriplePtr * {
		if (i < actualSize_) return &data_[i++];
		return nullptr;
	};
}

MutableTripleContainer::MutableGenerator TripleViewBatch::generator() {
	return [this, i = std::size_t(0)]() mutable -> TriplePtr * {
		if (i < actualSize_) return &data_[i++];
		return nullptr;
	};
}

namespace knowrob::py {
	template<>
	void createType<TripleContainer>() {
		using namespace boost::python;
		class_<TripleContainer, std::shared_ptr<TripleContainer>, boost::noncopyable>
				("TripleContainer", no_init)
				.def("__iter__",
					 boost::python::iterator<TripleContainer, boost::python::return_value_policy<boost::python::copy_const_reference>>{});
	}
}
