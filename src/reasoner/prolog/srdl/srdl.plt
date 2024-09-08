/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

 :- begin_rdf_tests('srdl', 'owl/robots/PR2.owl').

:- use_module('./srdl.pl').

:- rdf_register_prefix(test, 'http://knowrob.org/kb/PR2.owl#', [force(true)]).

:- end_tests('srdl').
