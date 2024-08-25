/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/storage/ObserverJob.h"

using namespace knowrob;

ObserverJob::ObserverJob(const std::shared_ptr<ObserverManager> &manager,
						const GraphQueryPtr &query,
						const AnswerHandler &callback)
	: manager_(manager), query_(query), callback_(callback) {
	// construct the observation graph given the query
	terminalNodes_ = createGraph(query->term(), {});
	// initialize the tables for the query by running a DB query
	// for each node in the observation graph.
	for (const auto &node : nodes_) {
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
			for (const auto &parent : parents) {
				node->parents.push_back(parent.get());
				parent->children.push_back(node);
			}
			return { node };
		}
		case GraphTermType::Builtin: {
			if (!parents.empty()) {
				for (const auto &parent : parents) {
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
			for (const auto &subTerm : sequence->terms()) {
				nextParents = createGraph(subTerm, nextParents);
			}
			return nextParents;
		}
		case GraphTermType::Union: {
			auto unionTerm = std::static_pointer_cast<GraphUnion>(term);
			ObserverJob::NodeParents nextParents;
			for (const auto &subTerm : unionTerm->terms()) {
				auto subParents = createGraph(subTerm, parents);
				nextParents.insert(nextParents.end(), subParents.begin(), subParents.end());
			}
			return nextParents;
		}
	}
}

GraphQueryPtr ObserverJob::makeQuery(const std::vector<Node*> &reverseSequence) {
	std::vector<std::shared_ptr<GraphTerm>> terms;
	for (auto it = reverseSequence.rbegin(); it != reverseSequence.rend(); ++it) {
		terms.push_back((*it)->pattern);
		for (const auto &builtin : (*it)->builtins) {
			terms.push_back(builtin);
		}
	}
	if (terms.size()==1) {
		return std::make_shared<GraphQuery>(terms[0]);
	} else {
		return std::make_shared<GraphQuery>(std::make_shared<GraphSequence>(terms));
	}
}

void ObserverJob::initializeNode(const std::shared_ptr<Node> &nodeToInit) {
	std::vector<GraphQueryPtr> queries;
	std::queue<std::pair<Node*, std::vector<Node*>>> queryQueue;
	queryQueue.push({ nodeToInit.get(), {} });

	// build queries from the node and its parents
	while (!queryQueue.empty()) {
		auto [node, seq] = queryQueue.front();
		queryQueue.pop();

		if (node->parents.empty()) {
			queries.push_back(makeQuery(seq));
		} else {
			for (const auto &parent : node->parents) {
				std::vector<Node*> newSeq = seq;
				newSeq.push_back(parent);
				queryQueue.push({ parent, newSeq });
			}
		}
	}

	// run queries against the database
	for (const auto &nodeQuery : queries) {
		manager_->query(nodeQuery, [this, nodeToInit](const AnswerPtr &answer) {
			initializeNode(nodeToInit, answer);
		});
	}
}

void ObserverJob::initializeNode(const std::shared_ptr<Node> &node, const AnswerPtr &answer) {
	auto answerHash = answer->hash();
	auto it = node->answers.find(answerHash);
	if (it == node->answers.end()) {
		// store the answer in the node
		node->answers[answerHash] = answer;
		// check if the node is a terminal node
		if (node->children.empty()) {
			// if so, call the callback
			callback_(answer);
		}
	}
}

bool ObserverJob::matches(const Node &node, const FramedTriple &triple) {
	auto &pat = node.pattern->value();
	return pat->hasInstance(triple);
}

void ObserverJob::processInsertion(const TripleContainerPtr &triples) {
	for (auto &triplePtr : *triples) {
		auto &triple = *triplePtr;
		for (auto &node : nodes_) {
			if (matches(*node, triple)) {
				insert(node, triple);
			}
		}
	}
}

void ObserverJob::insert(std::shared_ptr<Node> &node, const FramedTriple &triple) {
	// important here is to check the variable bindings of variables that appear
	// in parent nodes, as these can be expected to be bound when entering this node.
	// so if node pattern has a variable that appears in a parent node, then
	// the actual value of the triple must match the value of the variable in the parent node.
	// basically in this case select all the bindings from the parent nodes where the variable
	// is bound correctly and add the bindings to the current node plus any new bindings
	// in triple.
	// if the node is terminal, then call the callback with the new bindings.
	// if the node is not terminal, then a graph query must be constructed for each child node
	//  with the new bindings applied. If there are new solutions, then call insert on the child node
	//  with the new solutions to propagate the new data through the graph.
}

