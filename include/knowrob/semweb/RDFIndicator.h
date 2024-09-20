/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_RDF_INDICATOR_H
#define KNOWROB_RDF_INDICATOR_H

#include <optional>
#include <string_view>
#include "knowrob/formulas/Predicate.h"

namespace knowrob {
	/**
	 * A predicate indicator but in case of RDF predicates
	 * the functor can be a variable in which case the functor of
	 * the indicator is not defined.
	 */
	struct RDFIndicator {
		/**
		 * Create a new RDF indicator with the given predicate.
		 * @param predicate the predicate
		 */
		explicit RDFIndicator(const PredicatePtr &predicate);

		/**
		 * Create a new RDF indicator with the given functor and arity.
		 * @param functor the functor of the indicator
		 * @param arity the arity of the indicator
		 */
		RDFIndicator(std::string_view functor, size_t arity);

		/**
		 * Create a new RDF indicator with the given arity.
		 * @param arity the arity of the indicator
		 */
		explicit RDFIndicator(size_t arity);

		std::optional<std::string_view> functor;
		size_t arity;
	};

} // knowrob

#endif //KNOWROB_RDF_INDICATOR_H
