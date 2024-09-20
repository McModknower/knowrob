/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_ONTOLOGY_LANGUAGE_H
#define KNOWROB_ONTOLOGY_LANGUAGE_H

#include "string_view"

namespace knowrob::semweb {
	/**
	 * Used to indicate the file format when loading triple data.
	 */
	enum OntologyLanguage {
		RDFS,
		OWL
	};


	/**
	 * @param format string identifier of the data format.
	 * @return the corresponding enum value.
	 */
	OntologyLanguage ontologyLanguageFromString(std::string_view format);

	/**
	 * @param format the enum value.
	 * @return the corresponding string identifier.
	 */
	std::string_view ontologyLanguageToString(OntologyLanguage format);

	/**
	 * @param format the string identifier.
	 * @return true if the string is a valid ontology language.
	 */
	bool isOntologyLanguageString(std::string_view format);
}

#endif //KNOWROB_ONTOLOGY_LANGUAGE_H
