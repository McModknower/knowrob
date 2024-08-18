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
	if (!bulk->empty()) {
		bulk->execute();
	}

	// TODO: Can the update of property assertions be done more efficiently?
	// update property assertions
	std::set<std::string_view> visited;
	for (auto &assertion: subPropertyAssertions) {
		visited.insert(assertion.first);
	}
	bson_t pipelineDoc = BSON_INITIALIZER;
	for (auto &newProperty: visited) {
		bson_reinit(&pipelineDoc);

		bson_t pipelineArray;
		BSON_APPEND_ARRAY_BEGIN(&pipelineDoc, "pipeline", &pipelineArray);
		Pipeline pipeline(&pipelineArray);
		updateHierarchyP(pipeline,
						 tripleCollection_->name(),
						 rdfs::subPropertyOf->stringForm(),
						 newProperty);
		bson_append_array_end(&pipelineDoc, &pipelineArray);

		oneCollection_->evalAggregation(&pipelineDoc);
	}

	bson_destroy(&pipelineDoc);
}

void MongoTaxonomy::lookupParents(
		Pipeline &pipeline,
		const std::string_view &collection,
		const std::string_view &entity,
		const std::string_view &relation) {
	// lookup parent hierarchy.
	// e.g. for subClassOf these are all o* values of subClassOf documents of entity
	bson_t lookupArray;
	auto lookupStage = pipeline.appendStageBegin("$lookup");
	BSON_APPEND_UTF8(lookupStage, "from", collection.data());
	BSON_APPEND_UTF8(lookupStage, "as", "directParents");
	BSON_APPEND_ARRAY_BEGIN(lookupStage, "pipeline", &lookupArray);
	{
		Pipeline lookupPipeline(&lookupArray);
		// { $match: { s: $entity, p: $relation } }
		auto matchStage = lookupPipeline.appendStageBegin("$match");
		BSON_APPEND_UTF8(matchStage, "s", entity.data());
		BSON_APPEND_UTF8(matchStage, "p", relation.data());
		lookupPipeline.appendStageEnd(matchStage);
		// { $project: { "o*": 1 } }
		lookupPipeline.project("o*");
		// { $unwind: "$o*" }
		lookupPipeline.unwind("$o*");
	}
	bson_append_array_end(lookupStage, &lookupArray);
	pipeline.appendStageEnd(lookupStage);

	// convert "parents" field from list of documents to list of strings:
	// { $set { parents: { $map: { input: "$parents", "in": "$$this.o*" } } } }
	bson_t parentsDoc, mapDoc;
	auto setStage = pipeline.appendStageBegin("$set");
	BSON_APPEND_DOCUMENT_BEGIN(setStage, "directParents", &parentsDoc);
	{
		BSON_APPEND_DOCUMENT_BEGIN(&parentsDoc, "$map", &mapDoc);
		{
			BSON_APPEND_UTF8(&mapDoc, "input", "$directParents");
			BSON_APPEND_UTF8(&mapDoc, "in", "$$this.o*");
		}
		bson_append_document_end(&parentsDoc, &mapDoc);
	}
	bson_append_document_end(setStage, &parentsDoc);
	pipeline.appendStageEnd(setStage);
}


void MongoTaxonomy::updateHierarchyP(
		Pipeline &pipeline,
		const std::string_view &collection,
		const std::string_view &relation,
		const std::string_view &newChild) {
	// lookup hierarchy into array field "parents"
	lookupParents(pipeline, collection, newChild, relation);

	// lookup documents that include the child in the p* field
	bson_t lookupArray;
	auto lookupStage = pipeline.appendStageBegin("$lookup");
	BSON_APPEND_UTF8(lookupStage, "from", collection.data());
	BSON_APPEND_UTF8(lookupStage, "as", "doc");
	BSON_APPEND_ARRAY_BEGIN(lookupStage, "pipeline", &lookupArray);
	{
		Pipeline lookupPipeline(&lookupArray);
		// { $match: { "p*": $newChild } }
		auto matchStage = lookupPipeline.appendStageBegin("$match");
		BSON_APPEND_UTF8(matchStage, "p*", newChild.data());
		lookupPipeline.appendStageEnd(matchStage);
		// { $project: { "p*": 1 } }
		lookupPipeline.project("p*");
	}
	bson_append_array_end(lookupStage, &lookupArray);
	pipeline.appendStageEnd(lookupStage);
	pipeline.unwind("$doc");

	// add parents to the doc.p* field
	// { $set: { "doc.p*": { $setUnion: [ "$doc.p*", "$parents" ] } } }
	pipeline.setUnion("doc.p*", {"$doc.p*", "$directParents"});
	// make the "doc" field the new root
	pipeline.replaceRoot("$doc");
	// merge the result into the collection
	pipeline.merge(collection);
}
