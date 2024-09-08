/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

:- module(srdl,
    [
        action_feasible_on_robot/2,
        missing_for_action/4,
        missing_cap_for_action/3,
        missing_comp_for_action/3,
        required_cap_for_action/2,
        required_comp_for_action/2,
        cap_available_on_robot/2
  ]).
/** <module> Reasoning about robot components and capabilities

  @author Moritz Tenorth
  @license BSD
*/
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb')).

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2, 'http://knowrob.org/kb/srdl2.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2comp, 'http://knowrob.org/kb/srdl2-comp.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2cap, 'http://knowrob.org/kb/srdl2-cap.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2act, 'http://knowrob.org/kb/srdl2-action.owl#', [keep(true)]).

:- rdf_meta
        action_feasible_on_robot(r,r),
        missing_for_action(r,r,r,r),
        missing_cap_for_action(r,r,r),
        required_cap_for_action(r,r),
        missing_comp_for_action(r,r,r),
        required_comp_for_action(r,r),
        cap_available_on_robot(r,r).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Actions


%% action_feasible_on_robot(?Action, ?Robot).
%
% Verifies that an action is feasible on the given robot by making
% sure that neither capabilites nor components are missing.
%
% The 3-argument variant also checks restrictions on action parameters
% with the action instance described in ActionDescription.
%
% @param Action   Action class to be checked
% @param Robot   Robot instance to be checked
%
action_feasible_on_robot(ActionConcept, Robot) :-
	\+ missing_cap_for_action(ActionConcept, Robot, _),
	\+ missing_comp_for_action(ActionConcept, Robot, _).


%% missing_for_action(Action, Robot, MissingCaps, MissingComps).
%
% Determines all components and capabilites that are required by an action,
% but are not available on the given robot.
%
% @param Action   Action class to be checked
% @param Robot   Robot instance to be checked
%
missing_for_action(Action, Robot, MissingCaps, MissingComps) :-
	missing_cap_for_action(Action, Robot, MissingCaps);
	missing_comp_for_action(Action, Robot, MissingComps).


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Capabilities


%% missing_cap_for_action(Action, Robot, Cap) is nondet.
%
% Missing capabilites are required, but not available on the robot
%
% @param Action   Action class to be checked
% @param Robot   Robot instance to be checked
% @param Cap     Capability required to perform the action
%
missing_cap_for_action(Action, Robot, Cap) :-
	required_cap_for_action(Action, Cap),
	\+ cap_available_on_robot(Cap, Robot).


%% required_cap_for_action(Action, Cap) is nondet.
%
% Capabilities required by an action and all of its sub-actions
%
% @param Action   Action class to be checked
% @param Cap     Capability required to perform the action
%
required_cap_for_action(Action, Cap) :-
	sw_subclass_of_expr(Action, some(srdl2cap:'dependsOnCapability', Cap)).

required_cap_for_action(Action, Cap) :-
	sw_subclass_of_expr(Action, some(srdl2cap:'dependsOnCapability', Cp)),
	sw_subclass_of_expr(Cp, some(srdl2cap:'dependsOnCapability', Cap)).


%% cap_available_on_robot(Cap, Robot) is nondet.
%
% Check if a capability is available on a robot. This is the case if the capability
%
% 1) is asserted for this robot class
%
% 2) is asserted for this robot instance
%
% 3) depends only on available components and sub-capabilites
%
% @param Cap   Capability class to be checked
% @param Robot Robot instance
%
cap_available_on_robot(Cap, Robot) :-
	% capability asserted for robot instance
	sw_instance_of(Robot, knowrob:'Robot'),
	sw_triple(Robot, srdl2cap:'hasCapability', SubCap),
	sw_subclass_of(SubCap, Cap).

% capability asserted for robot class
cap_available_on_robot(Cap, Robot) :-
	sw_subclass_of(RobotClass, knowrob:'Robot'),
	sw_instance_of(Robot, RobotClass),
	sw_subclass_of_expr(RobotClass, some(srdl2cap:'hasCapability', SubCap)),
	% If sub-properties are available, their super-capabilites are also
	% available. Make sure, however, not to scale beyond 'Capability'.
	sw_subclass_of(SubCap, Cap),
	sw_subclass_of(Cap, srdl2cap:'Capability'),
	\+ rdf_equal(Cap, srdl2cap:'Capability').

% capability depends only on available components or capabilities
cap_available_on_robot(Cap, Robot) :-
	sw_instance_of(Robot, knowrob:'Robot'),
	sw_subclass_of(Cap, srdl2cap:'Capability'),
	\+ rdf_equal(Cap, srdl2cap:'Capability'),

	forall( sw_subclass_of_expr(Cap, some(srdl2comp:'dependsOnComponent', CompT)),
	        comp_type_available(Robot, CompT) ),

	forall( sw_subclass_of_expr(Cap, some(srdl2cap:'dependsOnCapability', SubCap)),
	        cap_available_on_robot(SubCap, Robot) ).


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Components

%% missing_comp_for_action(Action, Robot, Comp) is nondet.
%
% Missing components are required, but not available on the robot
%
% @param Action  Action class to be checked
% @param Robot   Robot instance to be checked
% @param Comp    Component required to perform the action
%
missing_comp_for_action(Action, Robot, Comp) :-
	required_comp_for_action(Action, Comp),
	\+ comp_type_available(Robot, Comp).


%% required_comp_for_action(Action, Comp) is nondet.
%
% Components that are either directly required by an action and
% all of its sub-actions, or indirectly required by required
% capabilities
%
% @param Action   Action class to be checked
% @param Comp     Component required to perform the action
%
required_comp_for_action(Action, Comp) :-
	% components directly required by an action and all of its sub-actions
	sw_subclass_of_expr(Action, some(srdl2comp:'dependsOnComponent', Comp)), atom(Comp).

% components indirectly required by required capabilities
required_comp_for_action(Action, Comp) :-
	required_cap_for_action(Action, Cap),
	sw_subclass_of_expr(Cap, some(srdl2comp:'dependsOnComponent', Comp)), atom(Comp).
