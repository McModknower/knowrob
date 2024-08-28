:- module(mongolog_units, [ strip_unit/5 ]).
/** <module> Handling of QUDT units in mongolog.

@author Daniel Beßler
@license BSD
*/

:- use_module('client',
	[ mng_strip_operator/3, mng_strip_type/3 ]).
:- use_module(library('ext/qudt'), [ qudt_unit/4 ]).

%%
% Enforce arithmetic operator.
%
arithmetic_operator(=,is) :- !.
arithmetic_operator(X,X).

%%
% Strip unit from value term for compile-time conditions.
% Only succeeds if unit information is provided in Term.
%
strip_unit(Term, Operator, Multiplier, Offset, Value) :-
	nonvar(Term),
	% strip opearator if any
	mng_strip_operator(Term, MngOperator, WithoutOperator),
	compound(WithoutOperator),
	% try to gather unit data
	WithoutOperator =.. [Symbol, WithoutUnit],
	qudt_unit(Symbol, _Kind, Multiplier, Offset),
	% make sure Operator is a known command that can be called
	arithmetic_operator(MngOperator, Operator),
	% remove type info (must be numeric)
	mng_strip_type(WithoutUnit, _, Value).
