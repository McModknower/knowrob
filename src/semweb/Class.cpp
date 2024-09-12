/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <queue>
#include <set>
#include "knowrob/semweb/Class.h"
#include "knowrob/Logger.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/semweb/ImportHierarchy.h"

using namespace knowrob::semweb;

Class::Class(std::string_view iri)
		: Resource(iri) {}

Class::Class(const IRIAtomPtr &iri)
		: Resource(iri) {}

bool Class::ClassComparator::operator()(const std::shared_ptr<Class> &lhs, const std::shared_ptr<Class> &rhs) const {
	return lhs->iri() < rhs->iri();
}

void Class::addDirectParent(const std::shared_ptr<Class> &directParent, std::optional<std::string_view> graph) {
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
}

void Class::removeDirectParent(const std::shared_ptr<Class> &directParent, std::optional<std::string_view> graph) {
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
		}
	}
}

bool Class::isDirectSubClassOf(const std::shared_ptr<Class> &directParent) {
	return directParents_.count(directParent) > 0;
}

bool Class::isSubClassOf(const std::shared_ptr<Class> &parent, bool includeSelf) {
	std::queue<Class *> queue_;
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

void Class::forallParents(const ClassVisitor &visitor,
						  bool includeSelf,
						  bool skipDuplicates) {
	std::queue<Class *> queue_;
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

void Class::forallChildren(const ClassTupleVisitor &visitor, bool skipDuplicates) {
	std::queue<std::pair<Class *, Class *>> queue_;
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

void Class::detach() {
	directParents_.clear();
	directChildren_.clear();
}

namespace knowrob::py {
	template<>
	void createType<semweb::Class>() {
		using namespace boost::python;

		class_<semweb::Class, bases<semweb::Resource>, std::shared_ptr<semweb::Class>, boost::noncopyable>
				("Class", init<std::string_view>())
				.def(init<const IRIAtomPtr &>())
				.def("addDirectParent", &semweb::Class::addDirectParent)
				.def("removeDirectParent", &semweb::Class::removeDirectParent)
				.def("directParents", &semweb::Class::directParents, return_value_policy<copy_const_reference>())
				.def("isDirectSubClassOf", &semweb::Class::isDirectSubClassOf)
				.def("isSubClassOf", &semweb::Class::isSubClassOf)
				.def("forallParents", &semweb::Class::forallParents);
	}
}

