
:- use_module(library('reasoner'), [ reasoner_define_relation/2 ]).
:- use_module(library('semweb/rdf_db'), [ rdf_register_prefix/3 ]).

:- rdf_register_prefix(lpn, 'http://knowrob.org/kb/lpn#', [force(true)]).
:- reasoner_define_relation('http://knowrob.org/kb/lpn#jealous', 2).

'http://knowrob.org/kb/lpn#jealous'(X,Y) :-
	rdf_has(X, 'http://knowrob.org/kb/lpn#loves', Z),
	rdf_has(Y, 'http://knowrob.org/kb/lpn#loves', Z).
