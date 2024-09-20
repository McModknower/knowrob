/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_GRAPH_TRANSFORMATION_RULE_H
#define KNOWROB_GRAPH_TRANSFORMATION_RULE_H

#include "knowrob/semweb/SPARQLQuery.h"

namespace knowrob {
	/**
	 * A graph transformation rule that restructures and possibly renames entities in the input graph.
	 */
	class GraphTransformationRule {
	public:
		/**
		 * @param from the statements to match in the input graph.
		 * @param to the statements to replace in the input graph.
		 */
		GraphTransformationRule(const std::vector<TriplePatternPtr> &from,
								const std::vector<TriplePatternPtr> &to)
				: from_(from), to_(to) {}

		/**
		 * @return the SPARQL query that is used to match the input graph.
		 */
		SPARQLQuery getSPARQLQuery() const { return SPARQLQuery(std::make_shared<GraphQuery>(from_)); }

		/**
		 * @return the statements to match in the input graph.
		 */
		const std::vector<TriplePatternPtr> &from() const { return from_; }

		/**
		 * @return the statements to replace in the input graph.
		 */
		const std::vector<TriplePatternPtr> &to() const { return to_; }

	protected:
		std::vector<TriplePatternPtr> from_;
		std::vector<TriplePatternPtr> to_;
	};

} // knowrob

#endif //KNOWROB_GRAPH_TRANSFORMATION_RULE_H
