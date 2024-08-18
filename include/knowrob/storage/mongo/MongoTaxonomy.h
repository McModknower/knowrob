/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_MONGO_TAXONOMY_H
#define KNOWROB_MONGO_TAXONOMY_H

#include <vector>
#include <string>
#include <memory>
#include "Pipeline.h"
#include "Collection.h"

namespace knowrob::mongo {
	/**
	 * A class to update the taxonomy in a MongoDB.
	 */
	class MongoTaxonomy {
	public:
		using StringPair = std::pair<std::string_view, std::string_view>;

		MongoTaxonomy(const std::shared_ptr<mongo::Collection> &tripleCollection,
					  const std::shared_ptr<mongo::Collection> &oneCollection,
					  const VocabularyPtr &vocabulary);

		void updateInsert(
				const std::vector<StringPair> &subClassAssertions,
				const std::vector<StringPair> &subPropertyAssertions);

		void updateRemove(
				const std::vector<StringPair> &subClassAssertions,
				const std::vector<StringPair> &subPropertyAssertions);

	protected:
		std::shared_ptr<mongo::Collection> tripleCollection_;
		std::shared_ptr<mongo::Collection> oneCollection_;
		VocabularyPtr vocabulary_;

		void update(
				const std::vector<StringPair> &subClassAssertions,
				const std::vector<StringPair> &subPropertyAssertions,
				bool isInsert);
	};

} // knowrob

#endif //KNOWROB_MONGO_TAXONOMY_H
