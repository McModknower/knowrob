/*
 * Copyright (c) 2022, Daniel Beßler
 * All rights reserved.
 *
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/reasoning/prolog/PrologReasoner.h>

using namespace knowrob;

// TODO: it seems all engines share the same data.
//           so when one loads a rule, it is available for all.
//           can this be controlled? here I would like that each PrologReasoner can have its
//           own set of data loaded.
//           so the PrologEngine would need to accept a data source parameter.

// TODO: support synchronization with data sources.
//      - when facts are asserted into EDB, also assert into PrologEngine
//      - support writing facts asserted from PrologEngine into EDB?
//           i.e. when assert is called in the PrologEngine.

PrologReasoner::PrologReasoner(const std::string &initFile)
: initFile_(initFile)
{
}

PrologReasoner::~PrologReasoner()
{
    // TODO: need to block until run finished?
}

void PrologReasoner::initialize()
{
    // claim a prolog engine where facts and rules can be asseted
    boost::shared_ptr<PrologEngine> engine = prologEnginePool_.claim();
    // consult the init file, i.e. load facts and rules declared
    // in the file, and execute directives it contains
    engine->consult(initFile_);
    // TODO: load any additional rules stored in IDBs
    //for(auto it=idbs_.begin(); it!=idbs_.end(); it++) {
    //}
    if(!idbs_.empty()) {
        // TODO: print warning "PrologEngine does not support the use of additional IDBs as of now."
    }
    // load any additional facts stored in EDBs
    for(auto it=edbs_.begin(); it!=ebds_.end(); it++) {
        boost::shared_ptr<IFactBase> edb = *it;
        Iterator<boost::shared_ptr<Predicate>> pit = edb.getFacts();
        while(pit.hasNext()) {
            boost::shared_ptr<Predicate> p = *pit;
            engine->assert(*p);
        }
    }
    // release the engine again
    prologEnginePool_.release(engine);
}

void PrologReasoner::consult(const std:string &prologFile)
{
    boost::shared_ptr<PrologEngine> engine = prologEnginePool_.claim();
    engine->consult(prologFile);
    prologEnginePool_.release(engine);
}

void PrologReasoner::assert(const Predicate &fact)
{
    boost::shared_ptr<PrologEngine> engine = prologEnginePool_.claim();
    engine->assert(fact);
    prologEnginePool_.release(engine);
}

void PrologReasoner::runQuery(const IQuery &goal, ReasoningStatus &status, MessageQueue<Answer> &answerQueue)
{
    // claim a prolog engine
    boost::shared_ptr<PrologEngine> engine = prologEnginePool_.claim();
    // run the query
    // TODO: might be prolog supports a better way to terminate a running
    //       inference. Here it can only be terminated in between two answers
    //       that are generated.
	engine->startQuery(goal, true);
	while(!status.isCancelled() && engine->hasMoreSolutions()) {
        answerQueue.push(engine->popSolution());
	}
	// TODO: need to block here? (flag=true means to block)
	engine->stopQuery(true);
    // release the engine again
    prologEnginePool_.release(engine);
}

bool PrologReasoner::canReasonAbout(const PredicateIndicator &predicate)
{
    // create "current_functor(Functor,Arity)" predicate
    PrologPredicate current_functor("consult",1);
    current_functor.setArgument(0, predicate.functor().c_str());
    current_functor.setArgument(1, predicate.arity());
    // run a query
    boost::shared_ptr<PrologEngine> engine = prologEnginePool_.claim();
    bool isCurrent = engine->oneSolution(boost::shared_ptr<IQuery>(new IQuery(current_functor)))->isTrue();
    prologEnginePool_.release(engine);
    return isCurrent;
}
