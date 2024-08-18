/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <mongoc.h>
#include <set>
#include "knowrob/storage/mongo/MongoTaxonomy.h"
#include "knowrob/storage/mongo/Pipeline.h"
#include "knowrob/semweb/rdfs.h"

using namespace knowrob;
using namespace knowrob::mongo;
using namespace knowrob::semweb;

MongoTaxonomy::MongoTaxonomy(
		const std::shared_ptr<mongo::Collection> &tripleCollection,
		const std::shared_ptr<mongo::Collection> &oneCollection,
		const VocabularyPtr &vocabulary)
		: tripleCollection_(tripleCollection), oneCollection_(oneCollection), vocabulary_(vocabulary) {
}

template<typename ResourceType>
static void bulkUpdateTaxonomy(
		std::shared_ptr<mongo::BulkOperation> &bulk,
		std::shared_ptr<Vocabulary> &vocabulary,
		std::string_view taxonomyRelation,
		const std::vector<MongoTaxonomy::StringPair> &assertions) {
	if (assertions.empty()) { return; }

	std::set<MongoTaxonomy::StringPair> invalidAssertions;
	for (auto &assertion: assertions) {
		// Note: the assertion itself must not be included as the Vocabulary class is
		//       used to build the o* field.
		auto resource = vocabulary->define<ResourceType>(assertion.first);
		resource->forallChildren([&invalidAssertions](const ResourceType &child, const ResourceType &directParent) {
			invalidAssertions.insert({child.iri(), directParent.iri()});
		});
	}

	for (auto &assertion: invalidAssertions) {
		bson_t query = BSON_INITIALIZER;
		BSON_APPEND_UTF8(&query, "s", assertion.first.data());
		BSON_APPEND_UTF8(&query, "p", taxonomyRelation.data());
		BSON_APPEND_UTF8(&query, "o", assertion.second.data());

		bson_t update = BSON_INITIALIZER;
		bson_t setDoc, setArray;
		BSON_APPEND_DOCUMENT_BEGIN(&update, "$set", &setDoc);
		BSON_APPEND_ARRAY_BEGIN(&setDoc, "o*", &setArray);

		auto cls = vocabulary->define<ResourceType>(assertion.second);
		uint32_t numParents = 0;
		cls->forallParents([&setArray, &numParents](const ResourceType &parent) {
			auto arrayKey = std::to_string(numParents++);
			BSON_APPEND_UTF8(&setArray, arrayKey.c_str(), parent.iri().data());
		}, true);

		bson_append_array_end(&setDoc, &setArray);
		bson_append_document_end(&update, &setDoc);

		bulk->pushUpdate(&query, &update);
	}
}

void MongoTaxonomy::update(
		const std::vector<StringPair> &subClassAssertions,
		const std::vector<StringPair> &subPropertyAssertions) {
	// below performs the server-side data transformation for updating hierarchy relations
	// such as rdf::type.

	if (subClassAssertions.empty() && subPropertyAssertions.empty()) return;

	std::set<std::string_view> invalidPropertyAssertions;
	for (auto &assertion: subPropertyAssertions) {
		invalidPropertyAssertions.insert(assertion.first);
	}

	// create a bulk operation for updating subClassOf and subPropertyOf relations
	// using the taxonomic assertions in the Vocabulary class
	auto bulk = tripleCollection_->createBulkOperation();

	// add bulk operations to update subClassOf and subPropertyOf relations
	if (!subClassAssertions.empty()) {
		bulkUpdateTaxonomy<Class>(bulk, vocabulary_, rdfs::subClassOf->stringForm(), subClassAssertions);
	}
	if (!subPropertyAssertions.empty()) {
		bulkUpdateTaxonomy<Property>(bulk, vocabulary_, rdfs::subPropertyOf->stringForm(), subPropertyAssertions);
	}

	// also update the p* field of all documents that contain an invalidated property in this field
	for (auto &invalidProperty: invalidPropertyAssertions) {
		// match all assertions where the property appears in the p* field
		bson_t query = BSON_INITIALIZER;
		BSON_APPEND_UTF8(&query, "p*", invalidProperty.data());

		// update the p* field by using $addToSet to add parents of the invalidated property to the p* field
		// 		{ $addToSet: { "p*": { $each: [ .... ] } } }
		bson_t update = BSON_INITIALIZER;
		bson_t addToSetDoc, addToSetEach, addToSetArray;
		BSON_APPEND_DOCUMENT_BEGIN(&update, "$addToSet", &addToSetDoc);
		BSON_APPEND_DOCUMENT_BEGIN(&addToSetDoc, "p*", &addToSetEach);
		BSON_APPEND_ARRAY_BEGIN(&addToSetEach, "$each", &addToSetArray);

		auto resource = vocabulary_->define<Property>(invalidProperty);
		uint32_t numParents = 0;
		resource->forallParents([&addToSetArray, &numParents](const Property &parent) {
			auto arrayKey = std::to_string(numParents++);
			BSON_APPEND_UTF8(&addToSetArray, arrayKey.c_str(), parent.iri().data());
		}, false);

		bson_append_array_end(&addToSetEach, &addToSetArray);
		bson_append_document_end(&addToSetDoc, &addToSetEach);
		bson_append_document_end(&update, &addToSetDoc);
		bulk->pushUpdate(&query, &update);
	}

	if (!bulk->empty()) {
		bulk->execute();
	}
}
