/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_ONTOLOGY_FILE_H
#define KNOWROB_ONTOLOGY_FILE_H

#include "knowrob/db/DataFile.h"
#include "knowrob/db/OntologySource.h"
#include "knowrob/semweb/TripleContainer.h"
#include "knowrob/semweb/TripleFormat.h"
#include "knowrob/semweb/OntologyLanguage.h"

namespace knowrob {
	/**
	 * An ontology file is a data source that provides ontology data in a file.
	 */
	class OntologyFile : public DataFile, public OntologySource {
	public:
		/**
		 * @param uri URI of the data source.
		 * @param format string identifier of the data format.
		 */
		OntologyFile(const URI &uri, std::string_view format);

		/**
		 * @return the format of the triples in the file.
		 */
		semweb::TripleFormat tripleFormat() const { return tripleFormat_; }

		/**
		 * @param language the language of the ontology.
		 */
		void setOntologyLanguage(semweb::OntologyLanguage language) { ontologyLanguage_ = language; }

		/**
		 * @return the language of the ontology.
		 */
		semweb::OntologyLanguage ontologyLanguage() const { return ontologyLanguage_; }

		// override DataSource
		DataSourceType type() const override { return DataSourceType::ONTOLOGY; }

	protected:
		semweb::TripleFormat tripleFormat_;
		semweb::OntologyLanguage ontologyLanguage_;
		std::optional<std::string> parentOrigin_;
	};

} // knowrob

#endif //KNOWROB_ONTOLOGY_FILE_H
