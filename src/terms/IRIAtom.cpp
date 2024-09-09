/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <boost/python.hpp>
#include "knowrob/terms/IRIAtom.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/semweb/Resource.h"
#include "knowrob/semweb/PrefixRegistry.h"
#include "knowrob/Logger.h"

using namespace knowrob;

PredicatePtr IRIAtom::operator()(const TermPtr &s, const TermPtr &o) const {
	auto functor = IRIAtom::Tabled(stringForm());
	return std::make_shared<Predicate>(functor, std::vector<TermPtr>{s, o});
}

PredicatePtr IRIAtom::operator()(const TermPtr &s) const {
	auto functor = IRIAtom::Tabled(stringForm());
	return std::make_shared<Predicate>(functor, std::vector<TermPtr>{s});
}

std::shared_ptr<knowrob::IRIAtom> IRIAtom::Tabled(std::string_view name) {
	auto it = Atom::table().find(name);
	if (it != table().end()) {
		if (auto atomPtr = it->second.value().lock()) {
			if (atomPtr->isIRI()) {
				return std::static_pointer_cast<IRIAtom>(atomPtr);
			}
		}
	}
	// Atom does not exist or was destroyed, create a new one
	auto inserted = table().emplace(name, std::nullopt);
	auto &jt = inserted.first;
	auto atom = std::make_shared<knowrob::IRIAtom>(jt->first);
	jt->second = atom;
	auto locked = jt->second.value().lock();
	if (!locked) {
		throw std::runtime_error("Failed to lock IRIAtom");
	}
	return std::static_pointer_cast<IRIAtom>(locked);
}

void IRIAtom::write(std::ostream &os) const {
	auto ns = semweb::Resource::iri_ns(stringForm_);
	if (!ns.empty()) {
		auto alias = PrefixRegistry::uriToAlias(ns);
		if (alias.has_value()) {
			auto name = semweb::Resource::iri_name(stringForm_);
			if (!name.empty()) {
				os << alias.value().get() << ":" << name;
				return;
			}
		}
	}
	Atom::write(os);
}

namespace knowrob {
	IRIAtomPtr iri(std::string_view ns, std::string_view name) {
		auto o_iri = PrefixRegistry::createIRI(ns, name);
		if (o_iri.has_value()) {
			return IRIAtom::Tabled(o_iri.value());
		}
		else {
			KB_WARN("Failed to create IRI");
			std::string fallback = std::string(ns) + "/" + std::string(name);
			return IRIAtom::Tabled(fallback);
		}
	}
}

namespace knowrob::py {
	template<>
	void createType<IRIAtom>() {
		using namespace boost::python;
		class_<IRIAtom, std::shared_ptr<IRIAtom>, bases<Atom, RDFNode>>("IRIAtom", no_init)
				.def("__init__", make_constructor(&IRIAtom::Tabled))
				.def("rdfNodeType", &IRIAtom::rdfNodeType)
				.def("isIRI", &IRIAtom::isIRI);
		def("iri", &iri);
	}
}
