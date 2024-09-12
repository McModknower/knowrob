/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <queue>
#include <set>
#include "knowrob/semweb/Property.h"
#include "knowrob/Logger.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/semweb/ImportHierarchy.h"

using namespace knowrob::semweb;

Property::Property(std::string_view iri)
		: Resource(iri), reification_(std::make_shared<Class>(reifiedIRI(iri))), flags_(0) {}

Property::Property(const IRIAtomPtr &iri)
		: Resource(iri), reification_(std::make_shared<Class>(reifiedIRI(iri->stringForm()))), flags_(0) {}

bool Property::PropertyComparator::operator()(const std::shared_ptr<Property> &lhs,
											  const std::shared_ptr<Property> &rhs) const {
	return lhs->iri() < rhs->iri();
}

knowrob::IRIAtomPtr Property::reifiedIRI(std::string_view iri) {
	// split the IRI at the last '#' and insert 'Reified' in between
	auto pos = iri.rfind('#');
	char delimiter = '#';
	if (pos == std::string::npos) {
		pos = iri.rfind('/');
		delimiter = '/';
	}
	std::stringstream ss;
	if (pos == std::string::npos) {
		ss << "Reified_" << iri;
		return IRIAtom::Tabled(ss.str());
	}
	ss << iri.substr(0, pos) << delimiter << "Reified_" << iri.substr(pos + 1);
	return IRIAtom::Tabled(ss.str());
}

knowrob::IRIAtomPtr Property::unReifiedIRI(std::string_view iri) {
	// split the IRI at the last '#' and remove 'Reified' in between
	auto pos = iri.rfind('#');
	char delimiter = '#';
	if (pos == std::string::npos) {
		pos = iri.rfind('/');
		delimiter = '/';
	}
	if (pos == std::string::npos) {
		return IRIAtom::Tabled(iri);
	}
	auto reified = iri.substr(pos + 1);
	if (reified.find("Reified_") != 0) {
		return IRIAtom::Tabled(iri);
	}
	std::stringstream ss;
	ss << iri.substr(0, pos) << delimiter << reified.substr(8);
	return IRIAtom::Tabled(ss.str());
}

void Property::addDirectParent(const std::shared_ptr<Property> &directParent, std::optional<std::string_view> graph) {
	auto graphAtom = graph_atom(graph);
	auto pair = directParents_.find(directParent);
	if (pair != directParents_.end()) {
		// add origin to list
		pair->second.insert(graphAtom);
	} else {
		// add new entry
		directParents_[directParent].insert(graphAtom);
		directParent->directChildren_.insert(shared_from_this());
	}
	reification_->addDirectParent(directParent->reification_, graph);
}

void
Property::removeDirectParent(const std::shared_ptr<Property> &directParent, std::optional<std::string_view> graph) {
	auto pair = directParents_.find(directParent);
	if (pair != directParents_.end()) {
		// remove origin from list
		std::string_view v_graph = (graph) ? graph.value() : ImportHierarchy::ORIGIN_SESSION;
		for (auto it = pair->second.begin(); it != pair->second.end(); it++) {
			if ((*it)->stringForm() == v_graph) {
				pair->second.erase(it);
				break;
			}
		}
		// remove if no origin is left
		if (pair->second.empty()) {
			directParents_.erase(pair);
			directParent->directChildren_.erase(shared_from_this());
			reification_->removeDirectParent(directParent->reification_, graph);
		}
	}
}

void Property::setInverse(const std::shared_ptr<Property> &inverse) {
	inverse_ = inverse;
}

bool Property::hasFlag(PropertyFlag flag) const {
	return flags_ & flag;
}

void Property::setFlag(PropertyFlag flag) {
	flags_ |= flag;
}

void Property::forallParents(const PropertyVisitor &visitor,
							 bool includeSelf,
							 bool skipDuplicates) {
	std::queue<Property *> queue_;
	std::set<std::string_view> visited_;

	// push initial elements to the queue
	if (includeSelf) queue_.push(this);
	else for (auto &x: directParents_) queue_.push(x.first.get());

	// visit each parent
	while (!queue_.empty()) {
		auto front = queue_.front();
		queue_.pop();
		// visit popped property
		visitor(*front);
		// remember visited nodes
		if (skipDuplicates) visited_.insert(front->iri());
		// push parents of visited property on the queue
		for (auto &directParent: front->directParents_) {
			if (skipDuplicates && visited_.count(directParent.first->iri()) > 0) continue;
			queue_.push(directParent.first.get());
		}
	}
}

void Property::forallChildren(const PropertyTupleVisitor &visitor, bool skipDuplicates) {
	std::queue<std::pair<Property *, Property *>> queue_;
	std::set<std::string_view> visited_;

	// push initial elements to the queue
	for (auto &x: directChildren_) queue_.emplace(x.get(), this);

	// visit each child
	while (!queue_.empty()) {
		auto pair = queue_.front();
		auto front_child = pair.first;
		auto front_parent = pair.second;
		queue_.pop();
		// visit popped property
		visitor(*front_child, *front_parent);
		// remember visited nodes
		if (skipDuplicates) visited_.insert(front_child->iri());
		// push children of visited property on the queue
		for (auto &directChild: front_child->directChildren_) {
			if (skipDuplicates && visited_.count(directChild->iri()) > 0) continue;
			queue_.emplace(directChild.get(), front_child);
		}
	}
}

bool Property::isSubPropertyOf(const std::shared_ptr<Property> &parent, bool includeSelf) {
	std::queue<Property *> queue_;
	std::set<std::string_view> visited_;

	if (includeSelf && this == parent.get()) return true;
	queue_.push(this);

	// visit each parent
	while (!queue_.empty()) {
		auto front = queue_.front();
		queue_.pop();

		// visit popped property
		if (front->directParents_.count(parent) > 0) return true;
		// remember visited nodes
		visited_.insert(front->iri());
		// push parents of visited property on the queue
		for (auto &directParent: front->directParents_) {
			if (visited_.count(directParent.first->iri()) > 0) continue;
			queue_.push(directParent.first.get());
		}
	}

	return false;
}

void Property::detach() {
	directParents_.clear();
	directChildren_.clear();
	inverse_.reset();
	reification_->detach();
}

namespace knowrob::py {
	template<>
	void createType<semweb::Property>() {
		using namespace boost::python;

		enum_<PropertyFlag>("PropertyFlag")
				.value("DATATYPE_PROPERTY", DATATYPE_PROPERTY)
				.value("ANNOTATION_PROPERTY", ANNOTATION_PROPERTY)
				.value("OBJECT_PROPERTY", OBJECT_PROPERTY)
				.value("TRANSITIVE_PROPERTY", TRANSITIVE_PROPERTY)
				.value("REFLEXIVE_PROPERTY", REFLEXIVE_PROPERTY)
				.value("SYMMETRIC_PROPERTY", SYMMETRIC_PROPERTY)
				.export_values();

		class_<semweb::Property, bases<semweb::Resource>, std::shared_ptr<semweb::Property>, boost::noncopyable>
				("Property", init<std::string_view>())
				.def(init<const IRIAtomPtr &>())
				.def("addDirectParent", &semweb::Property::addDirectParent)
				.def("removeDirectParent", &semweb::Property::removeDirectParent)
				.def("directParents", &semweb::Property::directParents, return_value_policy<copy_const_reference>())
				.def("setInverse", &semweb::Property::setInverse)
				.def("inverse", &semweb::Property::inverse, return_value_policy<copy_const_reference>())
				.def("hasFlag", &semweb::Property::hasFlag)
				.def("setFlag", &semweb::Property::setFlag)
				.def("isDatatypeProperty", &semweb::Property::isDatatypeProperty)
				.def("isAnnotationProperty", &semweb::Property::isAnnotationProperty)
				.def("isObjectProperty", &semweb::Property::isObjectProperty)
				.def("isTransitiveProperty", &semweb::Property::isTransitiveProperty)
				.def("isReflexiveProperty", &semweb::Property::isReflexiveProperty)
				.def("isSymmetricProperty", &semweb::Property::isSymmetricProperty)
				.def("forallParents", &semweb::Property::forallParents)
				.def("reification", &semweb::Property::reification)
				.def("reifiedIRI", &semweb::Property::reifiedIRI)
				.def("unReifiedIRI", &semweb::Property::unReifiedIRI);
	}
}
