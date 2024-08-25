/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/storage/ObserverJob.h"

using namespace knowrob;

ObserverJob::ObserverJob(const std::shared_ptr<ObserverManager> &manager,
						 const GraphQueryPtr &query,
						 const BindingsHandler &callback)
		: manager_(manager), query_(query), callback_(callback) {
	// construct the observation graph given the query
	terminalNodes_ = createGraph(query->term(), {});
	// initialize the tables for the query by running a DB query
	// for each node in the observation graph.
	for (const auto &node: nodes_) {
		initializeNode(node);
	}
}

ObserverJob::~ObserverJob() {
	stop();
	manager_ = nullptr;
}

void ObserverJob::stop() {
	nodes_.clear();
	terminalNodes_.clear();
}

std::shared_ptr<ObserverJob::Node> ObserverJob::createNode(const std::shared_ptr<GraphPattern> &pattern) {
	auto node = std::make_shared<Node>();
	node->pattern = pattern;
	nodes_.push_back(node);
	return node;
}

ObserverJob::NodeParents
ObserverJob::createGraph(const std::shared_ptr<GraphTerm> &term, const NodeParents &parents) {
	switch (term->termType()) {
		case GraphTermType::Pattern: {
			auto node = createNode(std::static_pointer_cast<GraphPattern>(term));
			for (const auto &parent: parents) {
				node->parents.push_back(parent.get());
				parent->children.push_back(node);
			}
			return {node};
		}
		case GraphTermType::Builtin: {
			if (!parents.empty()) {
				for (const auto &parent: parents) {
					auto builtin = std::static_pointer_cast<GraphBuiltin>(term);
					parent->builtins.push_back(builtin);
				}
			} else {
				KB_WARN("Builtins are not allowed at the top level of an observed query.");
			}
			return parents;
		}
		case GraphTermType::Sequence: {
			auto sequence = std::static_pointer_cast<GraphSequence>(term);
			ObserverJob::NodeParents nextParents = parents;
			for (const auto &subTerm: sequence->terms()) {
				nextParents = createGraph(subTerm, nextParents);
			}
			return nextParents;
		}
		case GraphTermType::Union: {
			auto unionTerm = std::static_pointer_cast<GraphUnion>(term);
			ObserverJob::NodeParents nextParents;
			for (const auto &subTerm: unionTerm->terms()) {
				auto subParents = createGraph(subTerm, parents);
				nextParents.insert(nextParents.end(), subParents.begin(), subParents.end());
			}
			return nextParents;
		}
	}
}

GraphQueryPtr ObserverJob::makeQuery(const std::vector<Node *> &reverseSequence) {
	std::vector<std::shared_ptr<GraphTerm>> terms;
	for (auto it = reverseSequence.rbegin(); it != reverseSequence.rend(); ++it) {
		terms.push_back((*it)->pattern);
		for (const auto &builtin: (*it)->builtins) {
			terms.push_back(builtin);
		}
	}
	if (terms.size() == 1) {
		return std::make_shared<GraphQuery>(terms[0]);
	} else {
		return std::make_shared<GraphQuery>(std::make_shared<GraphSequence>(terms));
	}
}

GraphQueryPtr ObserverJob::makeAtomicQuery(const std::shared_ptr<Node> &node, const BindingsPtr &bindings) {
	std::vector<std::shared_ptr<GraphTerm>> terms;
	terms.push_back(applyBindings(node->pattern, *bindings));
	for (const auto &builtin: node->builtins) {
		std::shared_ptr<GraphTerm> builtinTerm = builtin;
		terms.push_back(applyBindings(builtinTerm, *bindings));
	}
	if (terms.size() == 1) {
		return std::make_shared<GraphQuery>(terms[0]);
	} else {
		return std::make_shared<GraphQuery>(std::make_shared<GraphSequence>(terms));
	}
}

void ObserverJob::initializeNode(const std::shared_ptr<Node> &nodeToInit) {
	std::vector<GraphQueryPtr> queries;
	std::queue<std::pair<Node *, std::vector<Node *>>> queryQueue;
	queryQueue.push({nodeToInit.get(), {}});

	// build queries from the node and its parents
	while (!queryQueue.empty()) {
		auto [node, seq] = queryQueue.front();
		queryQueue.pop();

		if (node->parents.empty()) {
			queries.push_back(makeQuery(seq));
		} else {
			for (const auto &parent: node->parents) {
				std::vector<Node *> newSeq = seq;
				newSeq.push_back(parent);
				queryQueue.push({parent, newSeq});
			}
		}
	}

	// run queries against the database
	for (const auto &nodeQuery: queries) {
		manager_->query(nodeQuery, [this, nodeToInit](const BindingsPtr &bindings) {
			initializeNode(nodeToInit, bindings);
		});
	}
}

void ObserverJob::initializeNode(const std::shared_ptr<Node> &node, const BindingsPtr &answer) {
	auto answerHash = answer->hash();
	auto it = node->solutions.find(answerHash);
	if (it == node->solutions.end()) {
		// store the answer in the node
		node->solutions[answerHash] = answer;
		// check if the node is a terminal node
		if (node->children.empty()) {
			// if so, call the callback
			callback_(answer);
		}
	}
}

bool ObserverJob::matches(const Node &node, const FramedTriple &triple) {
	auto &pat = node.pattern->value();

	auto &s_pat = pat->subjectTerm();
	if (s_pat->isAtomic()) {
		auto s_pat_atom = std::static_pointer_cast<Atomic>(s_pat);
		if (s_pat_atom->stringForm() != triple.subject()) return false;
	} else if (!s_pat->isVariable()) {
		return false;
	}

	auto &p_pat = pat->propertyTerm();
	if (p_pat->isAtomic()) {
		auto p_pat_atom = std::static_pointer_cast<Atomic>(p_pat);
		if (p_pat_atom->stringForm() != triple.predicate()) return false;
	} else if (!p_pat->isVariable()) {
		return false;
	}

	auto &o_pat = pat->objectTerm();
	if (o_pat->isAtomic()) {
		auto o_pat_atom = std::static_pointer_cast<Atomic>(o_pat);

		if (triple.isObjectIRI() || triple.isObjectBlank()) {
			auto o_triple = triple.valueAsString();
			if (o_pat_atom->stringForm() != o_triple) return false;
		} else {
			auto o_triple = Atomic::makeTripleValue(triple);
			if (*o_pat_atom != *o_triple) return false;
		}
	} else if (!o_pat->isVariable()) {
		return false;
	}

	return true;
}

void ObserverJob::processInsertion(const TripleContainerPtr &triples) {
	for (auto &triplePtr: *triples) {
		auto &triple = *triplePtr;
		for (auto &node: nodes_) {
			if (matches(*node, triple)) {
				insert(node, triple);
			}
		}
	}
}

void ObserverJob::processRemoval(const TripleContainerPtr &triples) {
	KB_WARN("Removal of triples is not supported by the observer.");
}

void ObserverJob::insert(const std::shared_ptr<Node> &node, const FramedTriple &triple) {
	auto nodePattern = node->pattern->value();

	// compute bindings for the triple
	auto tripleBindings = std::make_shared<Bindings>();
	if (nodePattern->subjectTerm()->isVariable()) {
		auto var = std::static_pointer_cast<Variable>(nodePattern->subjectTerm());
		tripleBindings->set(var, std::make_shared<IRIAtom>(triple.subject()));
	}
	if (nodePattern->propertyTerm()->isVariable()) {
		auto var = std::static_pointer_cast<Variable>(nodePattern->propertyTerm());
		tripleBindings->set(var, std::make_shared<IRIAtom>(triple.predicate()));
	}
	if (nodePattern->objectTerm()->isVariable()) {
		auto var = std::static_pointer_cast<Variable>(nodePattern->objectTerm());
		auto o_triple = Atomic::makeTripleValue(triple);
		tripleBindings->set(var, o_triple);
	}

	insert(node, tripleBindings);
}

BindingsPtr ObserverJob::applyBuiltins(const std::shared_ptr<Node> &node, const BindingsPtr &bindings) {
	if (node->builtins.empty()) {
		return bindings;
	} else {
		auto bindings_rw = std::make_shared<Bindings>(*bindings);
		for (const auto &builtin: node->builtins) {
			builtin->apply(bindings_rw);
		}
		return bindings_rw;
	}
}

void ObserverJob::insert(const std::shared_ptr<Node> &node, const BindingsPtr &tripleBindings) {
	std::vector<BindingsPtr> newBindings;

	if (node->children.empty()) {
		// the node is a root node
		auto newNodeSol = applyBuiltins(node, tripleBindings);
		node->solutions[newNodeSol->hash()] = newNodeSol;
		newBindings.push_back(newNodeSol);
	} else {
		// the node is not a root node. In this case it is important here to check that the bindings of variables
		// that appear in parent nodes are consistent with `tripleBindings`.
		for (auto &parentNode: node->parents) {
			for (auto &parentSolPair: parentNode->solutions) {
				auto &parentSolution = parentSolPair.second;
				if (parentSolution->isConsistentWith(*tripleBindings)) {
					auto newNodeSol = std::make_shared<Bindings>(*parentSolution);
					newNodeSol->operator+=(*tripleBindings);
					auto newNodeSol_const = applyBuiltins(node, newNodeSol);
					node->solutions[newNodeSol_const->hash()] = newNodeSol_const;
					newBindings.push_back(newNodeSol_const);
				}
			}
		}
	}

	if (!node->children.empty()) {
		// if the node is terminal, then call the callback with the new bindings.
		for (auto &newBinding: newBindings) {
			callback_(newBinding);
		}
	} else {
		// if the node is not terminal, then a graph query must be constructed for each child node
		//  with the new bindings applied. If there are new solutions, then call insert on the child node
		//  with the new solutions to propagate the new data through the graph.

		for (auto &childNode: node->children) {
			for (auto &newBinding: newBindings) {
				auto newQuery = makeAtomicQuery(childNode, newBinding);
				manager_->query(newQuery, [this, childNode](const BindingsPtr &bindings) {
					insert(childNode, bindings);
				});
			}
		}
	}
}

