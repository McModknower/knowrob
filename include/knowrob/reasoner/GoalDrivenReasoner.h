/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_GOAL_DRIVEN_REASONER_H
#define KNOWROB_GOAL_DRIVEN_REASONER_H

#include "Reasoner.h"
#include "Goal.h"
#include "knowrob/formulas/PredicateIndicator.h"

namespace knowrob {
	/**
	 * An enumeration of reasoner features for goal-driven reasoning.
	 */
	enum class GoalDrivenReasonerFeature {
		/**
		 * The reasoner supports simple conjunctions.
		 * A simple conjunction is a conjunction of literals.
		 * If this feature is not enabled, the reasoner will only receive queries
		 * that contain a single literal.
		 */
		SupportsSimpleConjunctions = 0x01,
		/**
		 * The reasoner can ground literals in extensional knowledge, i.e. in the
		 * factual data contained in its storage backend.
		 * Note that if this feature is enabled, the reasoner is expected to
		 * provide all extensional groundings for a literal if not told otherwise.
		 */
		SupportsExtensionalGrounding = 0x02,
	};

	/**
	 * A reasoner that supports goal-driven reasoning.
	 * Goal-driven reasoning is a form of reasoning where the reasoner is asked to evaluate a query.
	 * This is in contrast to data-driven reasoning, where the reasoner is started and then infers
	 * additional knowledge from the data.
	 */
	class GoalDrivenReasoner : public Reasoner {
	public:
		GoalDrivenReasoner() : Reasoner(), features_(0) {}

		/**
		 * @return true if the reasoner supports a specific feature.
		 */
		bool hasFeature(GoalDrivenReasonerFeature feature) const;

		/**
		 * Enable a specific feature of the reasoner.
		 */
		void enableFeature(GoalDrivenReasonerFeature feature);

		/**
		 * Find out if the relation is defined by this reasoner.
		 * A defined relation is a relation that is known to the reasoner, and
		 * for which the reasoner can provide additional groundings when being queried.
		 * @param indicator a predicate indicator.
		 * @return true if the relation is currently defined by this reasoner.
		 */
		bool isRelationDefined(const PredicateIndicator &indicator);

		/**
		 * Find out if the class is defined by this reasoner.
		 * A defined class is a class that is known to the reasoner, and
		 * for which the reasoner can provide additional groundings when being queried.
		 * @param iri a class IRI.
		 * @return true if the class is currently defined by this reasoner.
		 */
		bool isClassDefined(const std::string_view &iri);

		/**
		 * Add a defined relation to the reasoner.
		 * @param indicator a predicate indicator.
		 */
		void defineRelation(const PredicateIndicator &indicator);

		/**
		 * Add a defined relation to the reasoner.
		 * @param iri a RDF predicate.
		 */
		void defineRelation(const IRIAtomPtr &iri);

		/**
		 * Remove a defined relation from the reasoner.
		 * @param indicator a predicate indicator.
		 */
		void undefineRelation(const PredicateIndicator &indicator);

		/**
		 * Add a defined class to the reasoner.
		 * @param indicator a predicate indicator.
		 */
		void defineClass(const IRIAtomPtr &iri);

		/**
		 * Remove a defined class from the reasoner.
		 * @param indicator a predicate indicator.
		 */
		void undefineClass(const IRIAtomPtr &iri);

		/**
		 * @return the set of defined relations.
		 */
		const auto &definedRelations() const { return definedRelations_; }

		/**
		 * @return the set of defined classes.
		 */
		const auto &definedClasses() const { return definedClasses_; }

		/**
		 * Evaluate a query with a reasoner.
		 * The query is represented by a formula, a context and an answer queue
		 * where results of the reasoning process can be added.
		 * The evaluation of the query must be performed synchronously,
		 * i.e. the answer queue must be filled before the function returns.
		 * A reasoner may instead throw an exception if the query cannot be evaluated,
		 * or return false to also indicate an error status.
		 * @param query the query to evaluate.
		 * @return true on success, false otherwise.
		 */
		virtual bool evaluate(GoalPtr query) = 0;

	protected:
		std::set<PredicateIndicator> definedRelations_;
		std::set<PredicateIndicator> definedClasses_;
		int features_;
	};

	/**
	 * A runner that can be used to evaluate a query in a thread pool.
	 */
	class ReasonerRunner : public ThreadPool::Runner {
	public:
		std::shared_ptr<GoalDrivenReasoner> reasoner;
		std::shared_ptr<Goal> query;

		ReasonerRunner() = default;

		// ThreadPool::Runner interface
		void run() override;

	private:
		void run_();
	};

	using GoalDrivenReasonerPtr = std::shared_ptr<GoalDrivenReasoner>;
	using DefiningReasoner = std::pair<GoalDrivenReasonerPtr,AtomPtr>;
}

#endif //KNOWROB_GOAL_DRIVEN_REASONER_H
