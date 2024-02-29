/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_GRAPH_RESTRUCTURING_H
#define KNOWROB_GRAPH_RESTRUCTURING_H

#include <redland.h>
#include "string"
#include "GraphTransformation.h"
#include "GraphTransformationRule.h"
#include "knowrob/db/RedlandModel.h"

namespace knowrob {
	/**
	 * A graph transformation that restructures and possibly renames entities in the input graph.
	 */
	class GraphRestructuring : public GraphTransformation {
	public:
		GraphRestructuring();

		/**
		 * Adds a rule to this transformation.
		 * @param rule a rule.
		 */
		void addRule(std::shared_ptr<GraphTransformationRule> rule);

		// override GraphTransformation
		bool configure(const boost::property_tree::ptree &opts) override;

		// override GraphTransformation
		void initializeTransformation() override;

		// override GraphTransformation
		void finalizeTransformation() override;

		// override GraphTransformation
		void pushInputTriples(const semweb::TripleContainerPtr &triples) override;

	protected:
		std::unique_ptr<RedlandModel> model_;
		std::vector<std::shared_ptr<GraphTransformationRule>> rules_;

		void doTransformation(GraphTransformationRule &rule);
	};

} // knowrob

#endif //KNOWROB_GRAPH_RESTRUCTURING_H
