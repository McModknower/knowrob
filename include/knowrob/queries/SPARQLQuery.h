/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_SPARQL_QUERY_H
#define KNOWROB_SPARQL_QUERY_H

#include "knowrob/semweb/FramedTriple.h"
#include "knowrob/semweb/FramedTriplePattern.h"

namespace knowrob {
	/**
	 * A SPARQL query.
	 */
	class SPARQLQuery {
	public:
		/**
		 * Create a SPARQL query that selects all triples matching the given pattern.
		 * @param triplePattern the pattern to match.
		 */
		explicit SPARQLQuery(const FramedTriplePattern &triplePattern);

		/**
		 * @param triplePatterns the patterns to match.
		 */
		explicit SPARQLQuery(const std::vector<FramedTriplePatternPtr> &triplePatterns);

		/**
		 * @return the query string.
		 */
		std::string_view operator()() const { return queryString_; }

		/**
		 * @return the query string as a unsigned C string.
		 */
		const unsigned char *
		asUnsignedString() const { return reinterpret_cast<const unsigned char *>(queryString_.c_str()); }

	protected:
		std::string queryString_;

		static void selectBegin(std::ostream &os);

		static void selectEnd(std::ostream &os);

		static void dot(std::ostream &os);

		static void where(std::ostream &os, const FramedTriplePattern &triplePattern);

		static void where(std::ostream &os, const TermPtr &term);
	};

} // knowrob

#endif //KNOWROB_SPARQL_QUERY_H
