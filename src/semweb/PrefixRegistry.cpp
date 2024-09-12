/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/semweb/PrefixRegistry.h"
#include "knowrob/semweb/PrefixProbe.h"
#include "knowrob/Logger.h"

using namespace knowrob;

PrefixRegistry::PrefixRegistry() {
	registerPrefix_("owl", "http://www.w3.org/2002/07/owl");
	registerPrefix_("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns");
	registerPrefix_("rdfs", "http://www.w3.org/2000/01/rdf-schema");
	registerPrefix_("xsd", "http://www.w3.org/2001/XMLSchema");
	registerPrefix_("qudt", "http://data.nasa.gov/qudt/owl/qudt");
	registerPrefix_("dul", "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl");
	registerPrefix_("soma", "http://www.ease-crc.org/ont/SOMA.owl");
	registerPrefix_("knowrob", "http://knowrob.org/kb/knowrob.owl");
}

PrefixRegistry &PrefixRegistry::get() {
	static PrefixRegistry singleton;
	return singleton;
}

static inline bool isDelimiter(char c) {
	return c == '#' || c == '/';
}

void PrefixRegistry::registerPrefix_(std::string_view prefix, std::string_view uri) {
	auto s_prefix = std::string(prefix.data());
	auto s_uri = std::string(uri.data());
	if (!isDelimiter(s_uri[s_uri.size() - 1])) {
		// auto-insert delimiter if none is present
		s_uri += '#';
	}
	KB_DEBUG("Registering prefix \"{}\" for URI {}", s_prefix, s_uri);
	uriToAlias_[s_uri] = s_prefix;
	aliasToURI_[s_prefix] = s_uri;
}

void PrefixRegistry::registerPrefix(std::string_view prefix, std::string_view uri) {
	get().registerPrefix_(prefix, uri);
}

OptionalStringRef PrefixRegistry::uriToAlias(std::string_view uri) {
	if (uri.empty()) {
		return std::nullopt;
	} else if (isDelimiter(uri[uri.size() - 1])) {
		auto it = get().uriToAlias_.find(uri);
		return it == get().uriToAlias_.end() ? std::nullopt : OptionalStringRef(it->second);
	} else {
		auto withDelimiter = std::string(uri) + '#';
		auto it = get().uriToAlias_.find(withDelimiter);
		return it == get().uriToAlias_.end() ? std::nullopt : OptionalStringRef(it->second);
	}
}

OptionalStringRef PrefixRegistry::aliasToUri(std::string_view alias) {
	if (alias.empty()) {
		return std::nullopt;
	} else {
		auto it = get().aliasToURI_.find(alias);
		return it == get().aliasToURI_.end() ? std::nullopt : OptionalStringRef(it->second);
	}
}

std::optional<std::string> PrefixRegistry::createIRI(std::string_view alias, std::string_view entityName) {
	auto uri = aliasToUri(alias);
	if (uri.has_value()) {
		std::stringstream os;
		os << uri.value().get() << entityName;
		return os.str();
	} else {
		return std::nullopt;
	}
}

std::vector<std::string_view> PrefixRegistry::getAliasesWithPrefix(std::string_view prefix) {
	auto range_it = get().aliasToURI_.equal_range(PrefixProbe{prefix});
	std::vector<std::string_view> result;
	for (auto it = range_it.first; it != range_it.second; ++it) {
		result.emplace_back(it->first.c_str());
	}
	return result;
}
