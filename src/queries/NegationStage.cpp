/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <utility>

#include "knowrob/queries/NegationStage.h"
#include "knowrob/KnowledgeBase.h"
#include "knowrob/formulas/ModalFormula.h"
#include "knowrob/storage/StorageInterface.h"
#include "knowrob/semweb/RDFIndicator.h"

using namespace knowrob;

NegationStage::NegationStage(const std::shared_ptr<KnowledgeBase> &kb, QueryContextPtr ctx)
		: TokenBroadcaster(),
		  kb_(kb),
		  ctx_(std::move(ctx)) {}

void NegationStage::pushToBroadcast(const TokenPtr &tok) {
	if (tok->tokenType() == TokenType::ANSWER_TOKEN) {
		auto answer = std::static_pointer_cast<const Answer>(tok);
		if (answer->isPositive()) {
			if (!succeeds(std::static_pointer_cast<const AnswerYes>(answer))) {
				return;
			}
		}
	}
	TokenBroadcaster::pushToBroadcast(tok);
}

PredicateNegationStage::PredicateNegationStage(const std::shared_ptr<KnowledgeBase> &kb,
											   const QueryContextPtr &ctx,
											   const std::vector<FirstOrderLiteralPtr> &negatedLiterals)
		: NegationStage(kb, ctx),
		  negatedLiterals_(negatedLiterals) {}

bool PredicateNegationStage::succeeds(const AnswerYesPtr &answer) {
	// here the idea is to issue individual queries for each negative literal.
	// each query produces an AnswerBufferPtr object.
	// the set of all results can then be checked for a certain property, e.g. that none or all
	// of the queries produced a result.
	std::vector<TokenBufferPtr> results;

	auto kg = kb_->getBackendForQuery();

	for (auto &pat: negatedLiterals_) {
		auto pat1 = applyBindings(pat, *answer->substitution());
		auto indicator = RDFIndicator(pat1->predicate());

		// create an instance of the pattern based on given substitution
		auto instance = std::make_shared<FirstOrderLiteral>(
				pat1->predicate(), pat1->isNegated());
		// for now evaluate positive variant of the pattern.
		// NOTE: for open-world semantics this cannot be done. open-world reasoner
		//       would need to receive negative literal instead.
		instance->setIsNegated(false);

		// check if the EDB contains positive lit, if so negation cannot be true
		if (indicator.arity <= 2) {
			auto rdfLiteral = std::make_shared<FramedTriplePattern>(
					instance->predicate(), instance->isNegated());
			rdfLiteral->setTripleFrame(ctx_->selector);
			results.push_back(kb_->edb()->getAnswerCursor(
					kg, std::make_shared<GraphPathQuery>(rdfLiteral, ctx_)));
		}

		// next check if positive lit is an IDB predicate, if so negation cannot be true.
		// get list of reasoner that define the literal
		if (indicator.functor) {
			auto l_reasoner = kb_->reasonerManager()->getReasonerForRelation(
					PredicateIndicator(*indicator.functor, indicator.arity));
			for (auto &r: l_reasoner) {
				results.push_back(ReasonerManager::evaluateQuery(r, {instance}, ctx_));
			}
		}
	}

	// go through results, ensure *all* sub-queries failed, only then the negation stage succeeds.
	for (auto &result: results) {
		auto resultQueue = result->createQueue();
		auto firstResult = resultQueue->pop_front();
		if (firstResult->tokenType() == TokenType::ANSWER_TOKEN) {
			if (std::static_pointer_cast<const Answer>(firstResult)->isPositive()) {
				return false;
			}
		}
	}

	return true;
}

ModalNegationStage::ModalNegationStage(const std::shared_ptr<KnowledgeBase> &kb,
									   const QueryContextPtr &ctx,
									   const std::vector<std::shared_ptr<ModalFormula>> &negatedModals)
		: NegationStage(kb, ctx),
		  negatedModals_(negatedModals) {}

bool ModalNegationStage::succeeds(const AnswerYesPtr &answer) {
	// here the idea is to issue individual queries for each negated modal.
	// for now the positive variants of the modals is evaluated, the modal negation is thought to be
	// true if none of the positive variants can be grounded.

	auto outputStream = std::make_shared<TokenBuffer>();

	// run "submitQuery" for each negated modal, and collect results in outputStream
	for (auto &modal: negatedModals_) {
		auto modalInstance = applyBindings(modal, *answer->substitution());
		auto modalOutput = kb_->submitQuery(modalInstance, ctx_);
		modalOutput >> outputStream;
		modalOutput->stopBuffering();
	}

	// wait for and pop first message from output stream
	auto resultQueue = outputStream->createQueue();
	auto firstResult = resultQueue->pop_front();
	// "EOS" indicates that no solution was found for any of the modals
	return firstResult->indicatesEndOfEvaluation();
}
