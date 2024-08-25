/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/queries/QueryPipeline.h"
#include "knowrob/triples/GraphPathQuery.h"
#include "knowrob/queries/DisjunctiveBroadcaster.h"
#include "knowrob/queries/RedundantAnswerFilter.h"
#include "knowrob/queries/QueryTree.h"
#include "knowrob/formulas/ModalFormula.h"
#include "knowrob/queries/ModalStage.h"
#include "knowrob/queries/NegationStage.h"
#include "knowrob/queries/ConjunctiveBroadcaster.h"
#include "knowrob/queries/QueryError.h"
#include "knowrob/KnowledgeBase.h"
#include "knowrob/reasoner/Computable.h"
#include "knowrob/semweb/RDFIndicator.h"

using namespace knowrob;

namespace knowrob {
	/**
	 * Comparator for sorting EDB predicates.
	 */
	struct EDBComparator {
		explicit EDBComparator(VocabularyPtr vocabulary) : vocabulary_(std::move(vocabulary)) {}

		bool operator()(const FramedTriplePatternPtr &a, const FramedTriplePatternPtr &b) const;

		VocabularyPtr vocabulary_;
	};

	/**
	 * Comparator for sorting IDB predicates.
	 */
	struct IDBComparator {
		explicit IDBComparator(VocabularyPtr vocabulary) : vocabulary_(std::move(vocabulary)) {}

		bool operator()(const ComputablePtr &a, const ComputablePtr &b) const;

		VocabularyPtr vocabulary_;
	};
}

static bool isMaterializedInEDB(const std::shared_ptr<KnowledgeBase> &kb, std::string_view property) {
	return kb->vocabulary()->frequency(property) > 0;
}

QueryPipeline::QueryPipeline(const std::shared_ptr<KnowledgeBase> &kb, const FormulaPtr &phi,
							 const QueryContextPtr &ctx) {
	auto outStream = std::make_shared<TokenBuffer>();

	// decompose input formula into parts that are considered in disjunction,
	// and thus can be evaluated in parallel.
	QueryTree qt(phi);
	for (auto &path: qt) {
		// each node in a path is either a predicate, a negated predicate,
		// a modal formula, or the negation of a modal formula.
		// each of these formula types is handled separately below.
		std::vector<FirstOrderLiteralPtr> posLiterals, negLiterals;
		std::vector<std::shared_ptr<ModalFormula>> posModals, negModals;

		// split path into positive and negative literals and modals
		for (auto &node: path.nodes()) {
			switch (node->type()) {
				case FormulaType::PREDICATE: {
					auto lit = std::make_shared<FirstOrderLiteral>(
							std::static_pointer_cast<Predicate>(node), false);
					posLiterals.push_back(lit);
					break;
				}

				case FormulaType::MODAL:
					posModals.push_back(std::static_pointer_cast<ModalFormula>(node));
					break;

				case FormulaType::NEGATION: {
					auto negation = (Negation *) node.get();
					auto negated = negation->negatedFormula();
					switch (negated->type()) {
						case FormulaType::PREDICATE: {
							auto lit = std::make_shared<FirstOrderLiteral>(
									std::static_pointer_cast<Predicate>(negated), true);
							negLiterals.push_back(lit);
							break;
						}
						case FormulaType::MODAL:
							negModals.push_back(std::static_pointer_cast<ModalFormula>(negated));
							break;
						default:
							throw QueryError("Unexpected negated formula type {} in QueryTree.", (int) negated->type());
					}
					break;
				}
				default:
					throw QueryError("Unexpected formula type {} in QueryTree.", (int) node->type());
			}
		}

		std::shared_ptr<TokenBroadcaster> lastStage;
		std::shared_ptr<TokenBuffer> firstBuffer;

		// first evaluate positive literals if any.
		// note that the first stage is buffered, so that the next stage can be added to the pipeline
		// and only after stopping the buffering messages will be forwarded to the next stage.
		if (posLiterals.empty()) {
			// if there are none, we still need to indicate begin and end of stream for the rest of the pipeline.
			// so we just push `GenericYes` (an empty substitution) followed by `EndOfEvaluation` and
			// feed these messages to the next stage.
			firstBuffer = std::make_shared<TokenBuffer>();
			lastStage = firstBuffer;
			auto channel = TokenStream::Channel::create(lastStage);
			channel->push(GenericYes());
			channel->push(EndOfEvaluation::get());
			addInitialStage(lastStage);
		} else {
			auto pathQuery = std::make_shared<ConjunctiveQuery>(posLiterals, ctx);
			auto subPipeline = std::make_shared<QueryPipeline>(kb, pathQuery);
			firstBuffer = std::make_shared<AnswerBuffer_WithReference>(subPipeline);
			*subPipeline >> firstBuffer;
			subPipeline->stopBuffering();
			lastStage = firstBuffer;
			addInitialStage(lastStage);
		}

		// --------------------------------------
		// Evaluate all positive modals in sequence.
		// --------------------------------------
		for (auto &posModal: posModals) {
			auto modalStage = std::make_shared<ModalStage>(kb, posModal, ctx);
			modalStage->selfWeakRef_ = modalStage;
			lastStage >> modalStage;
			lastStage = modalStage;
		}

		// --------------------------------------
		// Evaluate all negative literals in parallel.
		// --------------------------------------
		if (!negLiterals.empty()) {
			// run a dedicated stage where negated literals can be evaluated in parallel
			auto negLiteralStage = std::make_shared<PredicateNegationStage>(
					kb, ctx, negLiterals);
			lastStage >> negLiteralStage;
			lastStage = negLiteralStage;
		}

		// --------------------------------------
		// Evaluate all negative modals in parallel.
		// --------------------------------------
		if (!negModals.empty()) {
			// run a dedicated stage where negated modals can be evaluated in parallel
			auto negModalStage = std::make_shared<ModalNegationStage>(
					kb, ctx, negModals);
			lastStage >> negModalStage;
			lastStage = negModalStage;
		}

		lastStage >> outStream;
		firstBuffer->stopBuffering();
	}
	// Note: At this point outStream could already contain solutions, but these are buffered
	// such that they won't be lost during pipeline creation.

	// if there were multiple paths, consolidate answers from them.
	// e.g. if one yields no and the other true, the no should be ignored.
	if (qt.numPaths() > 1) {
		auto consolidator = std::make_shared<DisjunctiveBroadcaster>();
		outStream >> consolidator;
		finalStage_ = consolidator;
	} else {
		finalStage_ = outStream;
	}
	bufferStage_ = outStream;
}

QueryPipeline::QueryPipeline(const std::shared_ptr<KnowledgeBase> &kb, const ConjunctiveQueryPtr &conjunctiveQuery) {
	auto &allLiterals = conjunctiveQuery->literals();

	// --------------------------------------
	// split input literals into positive and negative literals.
	// negative literals are evaluated in parallel after all positive literals.
	// --------------------------------------
	std::vector<FirstOrderLiteralPtr> positiveLiterals, negativeLiterals;
	for (auto &l: allLiterals) {
		if (l->isNegated()) negativeLiterals.push_back(l);
		else positiveLiterals.push_back(l);
	}

	// --------------------------------------
	// split positive literals into edb-only and computable.
	// also associate list of reasoner to computable literals.
	// --------------------------------------
	std::vector<FramedTriplePatternPtr> edbOnlyLiterals;
	std::vector<ComputablePtr> computableLiterals;
	bool hasUnknownPredicate = false;
	for (auto &l: positiveLiterals) {
		auto indicator = RDFIndicator(l->predicate());

		// Find reasoner for the predicate.
		std::vector<DefiningReasoner> l_reasoner;
		if (indicator.functor) {
			l_reasoner = kb->reasonerManager()->findDefiningReasoner(
					PredicateIndicator(*indicator.functor, indicator.arity));
		}

		if (l_reasoner.empty()) {
			if (indicator.arity > 2) {
				KB_WARN("Predicate {} is not defined by any reasoner.", *l->predicate());
				hasUnknownPredicate = true;
			} else if (indicator.functor && !isMaterializedInEDB(kb, *indicator.functor)) {
				KB_WARN("Predicate {} is neither materialized in the EDB nor defined by a reasoner.", *l->predicate());
				hasUnknownPredicate = true;
			} else {
				auto rdfLiteral = std::make_shared<FramedTriplePattern>(
						l->predicate(), l->isNegated());
				rdfLiteral->setTripleFrame(conjunctiveQuery->ctx()->selector);
				edbOnlyLiterals.push_back(rdfLiteral);
			}
		} else {
			computableLiterals.push_back(std::make_shared<Computable>(*l, l_reasoner));
		}
	}

	if (hasUnknownPredicate) {
		// generate a "don't know" message and return.
		auto out = std::make_shared<TokenBuffer>();
		auto channel = TokenStream::Channel::create(out);
		auto dontKnow = std::make_shared<AnswerDontKnow>();
		channel->push(dontKnow);
		channel->push(EndOfEvaluation::get());
		finalStage_ = out;
		bufferStage_ = out;
		return;
	}

	// --------------------------------------
	// sort positive literals.
	// --------------------------------------
	std::sort(edbOnlyLiterals.begin(), edbOnlyLiterals.end(), EDBComparator(kb->vocabulary()));

	// --------------------------------------
	// run EDB query with all edb-only literals.
	// --------------------------------------
	std::shared_ptr<TokenBuffer> edbOut;
	if (edbOnlyLiterals.empty()) {
		edbOut = std::make_shared<TokenBuffer>();
		auto channel = TokenStream::Channel::create(edbOut);
		channel->push(GenericYes());
		channel->push(EndOfEvaluation::get());
	} else {
		auto edb = kb->getBackendForQuery();
		edbOut = kb->edb()->getAnswerCursor(edb,
											std::make_shared<GraphPathQuery>(edbOnlyLiterals, conjunctiveQuery->ctx()));
	}
	addInitialStage(edbOut);

	// --------------------------------------
	// handle positive IDB literals.
	// --------------------------------------
	std::shared_ptr<TokenBroadcaster> idbOut;
	if (computableLiterals.empty()) {
		idbOut = edbOut;
	} else {
		idbOut = std::make_shared<TokenBroadcaster>();
		// --------------------------------------
		// Compute dependency groups of computable literals.
		// --------------------------------------
		DependencyGraph dg;
		dg.insert(computableLiterals.begin(), computableLiterals.end());

		// --------------------------------------
		// Construct a pipeline for each dependency group.
		// --------------------------------------
		if (dg.numGroups() == 1) {
			auto &literalGroup = *dg.begin();
			auto sequence = createComputationSequence(kb, literalGroup.member_);
			createComputationPipeline(
					kb,
					sequence,
					edbOut,
					idbOut,
					conjunctiveQuery->ctx());
		} else {
			// there are multiple dependency groups. They can be evaluated in parallel.

			// combines sub-answers computed in different parallel steps
			auto answerCombiner = std::make_shared<ConjunctiveBroadcaster>();
			// create a parallel step for each dependency group
			for (auto &literalGroup: dg) {
				// --------------------------------------
				// Construct a pipeline for each dependency group.
				// --------------------------------------
				auto sequence = createComputationSequence(kb, literalGroup.member_);
				createComputationPipeline(
						kb,
						sequence,
						edbOut,
						answerCombiner,
						conjunctiveQuery->ctx());
			}
			answerCombiner >> idbOut;
		}
	}

	// --------------------------------------
	// Evaluate all negative literals in parallel.
	// --------------------------------------
	if (!negativeLiterals.empty()) {
		// run a dedicated stage where negated literals can be evaluated in parallel
		auto negStage = std::make_shared<PredicateNegationStage>(
				kb, conjunctiveQuery->ctx(), negativeLiterals);
		idbOut >> negStage;
		finalStage_ = negStage;
	} else {
		finalStage_ = idbOut;
	}
	bufferStage_ = edbOut;
}

QueryPipeline::~QueryPipeline() {
	for (auto &stage: initialStages_) {
		stage->close();
	}
	initialStages_.clear();
}

void QueryPipeline::addInitialStage(const std::shared_ptr<TokenStream> &stage) {
	initialStages_.push_back(stage);
}

void QueryPipeline::operator>>(const std::shared_ptr<TokenStream> &stage) {
	finalStage_ >> stage;
}

void QueryPipeline::stopBuffering() {
	bufferStage_->stopBuffering();
}

namespace knowrob {
	// used to sort dependency nodes in a priority queue.
	// the nodes are considered to be dependent on each other through free variables.
	// the priority value is used to determine which nodes should be evaluated first.
	struct DependencyNodeComparator {
		bool operator()(const DependencyNodePtr &a, const DependencyNodePtr &b) const {
			// prefer node with fewer variables
			if (a->numVariables() != b->numVariables()) {
				return a->numVariables() > b->numVariables();
			}
			// prefer node with fewer neighbors
			if (a->numNeighbors() != b->numNeighbors()) {
				return a->numNeighbors() > b->numNeighbors();
			}
			return a < b;
		}
	};

	struct DependencyNodeQueue {
		const DependencyNodePtr node_;
		std::priority_queue<DependencyNodePtr, std::vector<DependencyNodePtr>, DependencyNodeComparator> neighbors_;

		explicit DependencyNodeQueue(const DependencyNodePtr &node) : node_(node) {
			// add all nodes to a priority queue
			for (auto &neighbor: node->neighbors()) {
				neighbors_.push(neighbor);
			}
		}
	};
}

std::vector<ComputablePtr> QueryPipeline::createComputationSequence(
		const std::shared_ptr<KnowledgeBase> &kb,
		const std::list<DependencyNodePtr> &dependencyGroup) {
	// Pick a node to start with.
	auto comparator = IDBComparator(kb->vocabulary());
	DependencyNodePtr first;
	ComputablePtr firstComputable;
	for (auto &n: dependencyGroup) {
		auto computable_n =
				std::static_pointer_cast<Computable>(n->literal());
		if (!first || comparator(firstComputable, computable_n)) {
			first = n;
			firstComputable = computable_n;
		}
	}

	// remember visited nodes, needed for circular dependencies
	// all nodes added to the queue should also be added to this set.
	std::set<DependencyNode *> visited;
	visited.insert(first.get());

	std::vector<ComputablePtr> sequence;
	sequence.push_back(firstComputable);

	// start with a FIFO queue only containing first node
	std::deque<std::shared_ptr<DependencyNodeQueue>> queue;
	auto qn0 = std::make_shared<DependencyNodeQueue>(first);
	queue.push_front(qn0);

	// loop until queue is empty and process exactly one successor of
	// the top element in the FIFO in each step. If an element has no
	// more successors, it can be removed from queue.
	// Each successor creates an additional node added to the top of the FIFO.
	while (!queue.empty()) {
		auto front = queue.front();

		// get top successor node that has not been visited yet
		DependencyNodePtr topNext;
		while (!front->neighbors_.empty()) {
			auto topNeighbor = front->neighbors_.top();
			front->neighbors_.pop();

			if (visited.count(topNeighbor.get()) == 0) {
				topNext = topNeighbor;
				break;
			}
		}
		// pop element from queue if all neighbors were processed
		if (front->neighbors_.empty()) {
			queue.pop_front();
		}

		if (topNext) {
			// push a new node onto FIFO
			auto qn_next = std::make_shared<DependencyNodeQueue>(topNext);
			queue.push_front(qn_next);
			sequence.push_back(std::static_pointer_cast<Computable>(topNext->literal()));
			visited.insert(topNext.get());
		}
	}

	return sequence;
}

static inline std::vector<FirstOrderLiteralPtr> replaceFunctors(const std::vector<ComputablePtr> &computables) {
	// Replace functors of computable literals with the more specific functors defined by the reasoner.
	std::vector<FirstOrderLiteralPtr> result;
	result.reserve(computables.size());
	for (auto &computable: computables) {
		auto computableFunctor = computable->reasonerList().front().second;

		if (computable->functor()->stringForm() == computableFunctor->stringForm()) {
			result.push_back(computable);
		} else {
			auto computablePredicate = std::make_shared<Predicate>(computableFunctor, computable->predicate()->arguments());
			result.push_back(std::make_shared<FirstOrderLiteral>(
					computablePredicate, computable->isNegated()));
		}
	}
	return result;
}

void QueryPipeline::createComputationPipeline(
		const std::shared_ptr<KnowledgeBase> &kb,
		std::vector<ComputablePtr> &computableLiterals,
		const std::shared_ptr<TokenBroadcaster> &pipelineInput,
		const std::shared_ptr<TokenBroadcaster> &pipelineOutput,
		const QueryContextPtr &ctx) {
	// This function generates a query pipeline for literals that can be computed
	// (EDB-only literals are processed separately). The literals are part of one dependency group.
	// They are sorted, and also evaluated in this order. For each computable literal there is at
	// least one reasoner that can compute the literal. However, instances of the literal may also
	// occur in the EDB. Hence, computation results must be combined with results of an EDB query
	// for each literal.

	struct MergedComputables {
		MergedComputables() : requiresEDB(true) {};
		ComputablePtr item;
		bool requiresEDB;
		std::vector<ComputablePtr> literals;
	};

	auto lastOut = pipelineInput;

	// --------------------------------------
	// Build conjunctive queries if possible.
	// To this end, search through the list of computable literals and find literals that can
	// be merged. This is only possible if:
	// - the literals share the same reasoner
	// - the literals have only one reasoner associated
	//   (else merge would rather generate a complex sub-pipeline)
	// - the reasoner supports simple conjunctions
	// - the literals are not stored in the EDB or the reasoner mirrors the EDB
	//   (else merge would rather generate a complex sub-pipeline)
	// --------------------------------------
	std::vector<MergedComputables> mergedComputables;
	while (!computableLiterals.empty()) {
		auto next = computableLiterals.front();
		auto indicator = RDFIndicator(next->predicate());
		computableLiterals.erase(computableLiterals.begin());

		auto &merged = mergedComputables.emplace_back();
		merged.item = next;
		// EDB queries are only required if one of the reasoner does not mirror the EDB,
		// i.e. one reasoner that produces all EDB results in addition to the IDB results.
		merged.requiresEDB = false;
		for (auto &r: next->reasonerList()) {
			// Note that we assume here that the data storage of the reasoner mirrors the EDB,
			// so we just check if the reasoner can ground literals in its storage.
			if (!r.first->hasFeature(GoalDrivenReasonerFeature::SupportsExtensionalGrounding)) {
				merged.requiresEDB = true;
				break;
			}
		}
		// The predicate can only be materialized in the EDB if it has at most two arguments,
		// i.e. if it is a RDF predicate.
		merged.requiresEDB = merged.requiresEDB && indicator.arity <= 2;
		if (merged.requiresEDB && indicator.functor) {
			// switch flag to false in case the literal is not materialized in the EDB
			merged.requiresEDB = isMaterializedInEDB(kb, *indicator.functor);
		}
		merged.literals.push_back(next);

		bool supportsSimpleConjunction = true;
		for (auto &r: next->reasonerList()) {
			if (!r.first->hasFeature(GoalDrivenReasonerFeature::SupportsSimpleConjunctions)) {
				supportsSimpleConjunction = false;
				break;
			}
		}

		if (supportsSimpleConjunction && !merged.requiresEDB && next->reasonerList().size() == 1) {
			// merge literals that can be computed by the same reasoner
			for (auto it = computableLiterals.begin(); it != computableLiterals.end();) {
				auto &lit = *it;
				if (next->reasonerList() == lit->reasonerList()) {
					merged.literals.push_back(lit);
					it = computableLiterals.erase(it);
				} else {
					++it;
				}
			}
		}
	}

	// finally build the pipeline
	for (auto &mergedComputable: mergedComputables) {
		auto stepInput = lastOut;
		auto stepOutput = std::make_shared<TokenBroadcaster>();
		uint32_t numStages = 0;

		// --------------------------------------
		// Construct a pipeline that grounds the literal in the EDB.
		// --------------------------------------
		if (mergedComputable.requiresEDB) {
			auto edb = kb->getBackendForQuery();
			auto edbStage = std::make_shared<TypedQueryStage<FirstOrderLiteral>>(
					ctx,
					mergedComputable.item,
					[kb, edb, ctx](const FirstOrderLiteralPtr &q) {
						auto rdfLiteral = std::make_shared<FramedTriplePattern>(
								q->predicate(), q->isNegated());
						rdfLiteral->setTripleFrame(ctx->selector);
						return kb->edb()->getAnswerCursor(edb, std::make_shared<GraphPathQuery>(rdfLiteral, ctx));
					});
			edbStage->selfWeakRef_ = edbStage;
			stepInput >> edbStage;
			edbStage >> stepOutput;
			++numStages;
		}

		// --------------------------------------
		// Construct a pipeline that grounds the literal in the IDB.
		// To this end add an IDB stage for each reasoner that defines the literal.
		// --------------------------------------
		for (auto &r: mergedComputable.item->reasonerList()) {
			auto idbStage = std::make_shared<TypedQueryStageVec<Computable>>(
					ctx, mergedComputable.literals,
					[r, ctx](const std::vector<ComputablePtr> &q) {
						return ReasonerManager::evaluateQuery(r.first, replaceFunctors(q), ctx);
					});
			idbStage->selfWeakRef_ = idbStage;
			stepInput >> idbStage;
			idbStage >> stepOutput;
			++numStages;
		}
		lastOut = stepOutput;

		// --------------------------------------
		// add a stage that consolidates the results of the EDB and IDB stages.
		// in particular the case needs to be handled where none of the stages return
		// 'true'. Also print a warning if two stages disagree but state they are confident.
		// --------------------------------------
		if (numStages > 1) {
			auto consolidator = std::make_shared<DisjunctiveBroadcaster>();
			lastOut >> consolidator;
			lastOut = consolidator;
		}

		// --------------------------------------
		// Optionally add a stage to the pipeline that drops all redundant result.
		// The filter is applied here to remove redundancies early on directly after IDB and EDB
		// results are combined.
		// --------------------------------------
		if (ctx->queryFlags & QUERY_FLAG_UNIQUE_SOLUTIONS) {
			auto filterStage = std::make_shared<RedundantAnswerFilter>();
			lastOut >> filterStage;
			lastOut = filterStage;
		}
	}

	lastOut >> pipelineOutput;
}

bool knowrob::EDBComparator::operator()(const FramedTriplePatternPtr &a, const FramedTriplePatternPtr &b) const {
	// - prefer evaluation of literals with fewer variables
	auto numVars_a = a->numVariables();
	auto numVars_b = b->numVariables();
	if (numVars_a != numVars_b) return (numVars_a > numVars_b);

	// - prefer literals with grounded predicate
	bool hasProperty_a = (a->propertyTerm() && a->propertyTerm()->termType() == TermType::ATOMIC);
	bool hasProperty_b = (b->propertyTerm() && b->propertyTerm()->termType() == TermType::ATOMIC);
	if (hasProperty_a != hasProperty_b) return (hasProperty_a < hasProperty_b);

	// - prefer properties that appear less often in the EDB
	if (hasProperty_a) {
		auto numAsserts_a = vocabulary_->frequency(
				std::static_pointer_cast<Atomic>(a->propertyTerm())->stringForm());
		auto numAsserts_b = vocabulary_->frequency(
				std::static_pointer_cast<Atomic>(b->propertyTerm())->stringForm());
		if (numAsserts_a != numAsserts_b) return (numAsserts_a > numAsserts_b);
	}

	return (a < b);
}

bool knowrob::IDBComparator::operator()(const ComputablePtr &a, const ComputablePtr &b) const {
	// - prefer evaluation of literals with fewer variables
	auto numVars_a = a->numVariables();
	auto numVars_b = b->numVariables();
	if (numVars_a != numVars_b) return (numVars_a > numVars_b);

	auto indicator_a = RDFIndicator(a->predicate());
	auto indicator_b = RDFIndicator(b->predicate());

	// - prefer literals with grounded predicate
	bool hasProperty_a = indicator_a.functor.has_value();
	bool hasProperty_b = indicator_b.functor.has_value();
	if (hasProperty_a != hasProperty_b) return (hasProperty_a < hasProperty_b);

	// - prefer literals with EDB assertions over literals without
	if (hasProperty_a) {
		auto hasEDBAssertion_a = vocabulary_->isDefinedProperty(*indicator_a.functor);
		auto hasEDBAssertion_b = vocabulary_->isDefinedProperty(*indicator_b.functor);
		if (hasEDBAssertion_a != hasEDBAssertion_b) return (hasEDBAssertion_a < hasEDBAssertion_b);
	}

	// - prefer properties that appear less often in the EDB
	if (hasProperty_a) {
		auto numAsserts_a = vocabulary_->frequency(*indicator_a.functor);
		auto numAsserts_b = vocabulary_->frequency(*indicator_b.functor);
		if (numAsserts_a != numAsserts_b) return (numAsserts_a > numAsserts_b);
	}

	// - prefer literals with fewer reasoner
	auto numReasoner_a = a->reasonerList().size();
	auto numReasoner_b = b->reasonerList().size();
	if (numReasoner_a != numReasoner_b) return (numReasoner_a > numReasoner_b);

	return (a < b);
}
