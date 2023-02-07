/*
 * Copyright (c) 2022, Daniel Beßler
 * All rights reserved.
 *
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/reasoner/DefinedPredicate.h"

using namespace knowrob;

DefinedPredicate::DefinedPredicate(const std::shared_ptr<PredicateIndicator> &indicator)
: indicator_(indicator),
  predicateType_(PredicateType::BUILT_IN)
{
}

bool DefinedPredicate::addReasoner(
		const std::shared_ptr<DefinedReasoner> &managedReasoner,
		const std::shared_ptr<PredicateDescription> &description)
{
	if(reasonerEnsemble_.empty()) {
		predicateType_ = description->type();
	}
	else if(predicateType_ != description->type()) {
		// another reasoner has this predicate defined with another type!
		return false;
	}
	reasonerEnsemble_.insert(managedReasoner);
	return true;
}
