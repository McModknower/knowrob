:- use_module('srdl').
:- use_module(library('semweb'), [ sw_register_computable/1,
                                   sw_register_prefix/2 ]).
:- use_module(library('logging')).

%%
% computable for srdl2-cap.owl#hasCapability
%%
:- sw_register_computable(srdl2cap:hasCapability).

srdl2cap:hasCapability(Robot,Capability) :-
	cap_available_on_robot(Capability, Robot).

%%
% computable for srdl2-cap.owl#dependsOnCapability
%%
:- sw_register_computable(srdl2cap:dependsOnCapability).

srdl2cap:dependsOnCapability(Action, Capability) :-
	required_cap_for_action(Action, Capability).
