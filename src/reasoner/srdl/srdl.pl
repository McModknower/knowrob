/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

:- module(srdl,
    [
        action_feasible_on_robot/2,
        action_feasible_on_robot/3,
        action_feasible_with_components/4,
        missing_for_action/4,
        missing_cap_for_action/3,
        missing_comp_for_action/3,
        required_cap_for_action/2,
        required_comp_for_action/2,
        insufficient_comp_for_action/4,
        unsatisfied_restr_for_action/5,
        unsatisfied_restr_for_required_comp/5,
        unsatisfied_restr_for_comp/5,
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
        action_feasible_with_components(r,r,r,-),
        missing_for_action(r,r,r,r),
        missing_cap_for_action(r,r,r),
        required_cap_for_action(r,r),
        missing_comp_for_action(r,r,r),
        insufficient_comp_for_action(r,r,r,r),
        required_comp_for_action(r,r),
        unsatisfied_restr_for_action(r,r,r,r,-),
        unsatisfied_restr_for_required_comp(r,r,r,r,-),
        unsatisfied_restr_for_comp(r,r,r,r,-),
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


%% action_feasible_with_components(+ActionC, +ActionI, +Robot, -Components).
%
% True for feasible actions of type ActionC for which a
% Components list exists such that each element is one of the required components
% without any unsatisfied restrictions imposed on the action described by ActionI.
%
% @param ActionC   Action class to be checked
% @param ActionI   Action individual to be checked
% @param Robot   Robot instance to be checked
% @param Components   List of components feasible for the action
%
action_feasible_with_components(ActionC, ActionI, Robot, Components) :-
	action_feasible_on_robot(ActionC, ActionI, Robot),
	findall(Cs, (
		unsatisfied_restr_for_required_comp(ActionC, ActionI, Robot, _, RestrictedComponents),
		findall(Comp, member((Comp,[]), RestrictedComponents), Cs),
		\+ Cs=[]
	), Components).


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
% Restricted actions

%% unsatisfied_restr_for_action(ActionC, ActionI, Robot, CompC, Unsatisfied).
%
% Checks if any action restriction imposed on robot or component level
% is unsatisfied.
%
% @param ActionC   Action class to be checked
% @param ActionI   Action instance to be checked
% @param Robot   Robot instance to be checked
% @param CompC   Required component class with unsatisfied restrictions
% @param Unsatisfied   Tuples of component individuals and unsatisfied restrictions
%
unsatisfied_restr_for_action(ActionC, ActionI, Robot, CompC, Unsatisfied) :-
	findall((CompC, Unsatisfied), (
		((
			% check restrictions on robot level
			unsatisfied_restr_for_comp(ActionC, ActionI, Robot, Robot, Rs),
			CompC='http://knowrob.org/kb/knowrob.owl#Robot',
			Unsatisfied=[(Robot,Rs)]);
			% check restrictions on component level
			unsatisfied_restr_for_required_comp(ActionC, ActionI, Robot, CompC, Unsatisfied)
		),
		not( member((_,[]), Unsatisfied) )
	), Xs),
	member((CompC, Unsatisfied), Xs).


%% unsatisfied_restr_for_required_comp(ActionC, ActionI, Robot, CompC, RestrictedComponents).
%
% Checks if any action restriction imposed on component level
% is unsatisfied.
%
% @param ActionC   Action class to be checked
% @param ActionI   Action instance to be checked
% @param Robot   Robot instance to be checked
% @param CompC   Required component class with unsatisfied restrictions
% @param RestrictedComponents   Tuples of component and unsatisfied restrictions
%
unsatisfied_restr_for_required_comp(ActionC, ActionI, Robot, CompC, RestrictedComponents) :-
	findall((CompC, RestrictedComponents), (
		required_comp_for_action(ActionC, CompC),
		findall((Comp,Restr), (
			rdf_reachable(Robot, 'http://knowrob.org/kb/srdl2-comp.owl#subComponent', Comp),
			once(sw_instance_of(Comp, CompC)),
			unsatisfied_restr_for_comp(ActionC, ActionI, Robot, Comp, Restr)
		), RestrictedComponents)
	), Xs),
	member((CompC, RestrictedComponents), Xs).


%% unsatisfied_restr_for_comp(ActionC, ActionI, Robot, Comp, Unsatisfied).
%
% Checks if any action restriction is unsatisfied.
%
% @param ActionC   Action class to be checked
% @param ActionI   Action instance to be checked
% @param Robot   Robot instance to be checked
% @param Comp   Component to be chacked
% @param Unsatisfied   List of unsatsfied action restrictions
%
unsatisfied_restr_for_comp(ActionC, ActionI, Robot, Comp, Unsatisfied) :-
	findall(R, (
		comp_restricted_action(Comp, ActionC, R),
		\+ sw_instance_of(ActionI, R)
	), Unsatisfied).


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


%% insufficient_comp_for_action(+ActionC, +ActionI, +Robot, ?Comp) is nondet.
%
% Insufficient components are required, but not available on the robot
% or available but not satisfying some restrictions imposed on the action
% (e.g., weak arms can't lift heavy objects).
%
% @param ActionC   Action class to be checked
% @param ActionI   Action individual to be checked
% @param Robot   Robot instance to be checked
% @param Comp    Component required to perform the action
%
insufficient_comp_for_action(ActionC, _, Robot, Comp) :-
	missing_comp_for_action(ActionC, Robot, Comp).

insufficient_comp_for_action(ActionC, ActionI, Robot, Comp) :-
	findall(Comp, (
		unsatisfied_restr_for_required_comp(ActionC, ActionI, Robot, Comp, Unsatisfied),
		\+ member((_,[]), Unsatisfied)
	), Cs),
	member(Comp, Cs).


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


%% comp_restricted_action(+Comp, ?ActionC, ?RestrictedC) is nondet.
%
% Yields restricted action classes of components that are subclass
% of the action class.
%
% @param Comp     Component required to perform the action
% @param ActionC   Action class to be checked
% @param RestrictedC   Restricted action class possible with component
%
comp_restricted_action(Comp, ActionC, RestrictedC) :-
	findall(R, (
		sw_instance_of(Comp, CompC),
		sw_subclass_of_expr(CompC, some(knowrob:'actionable', R)), atom(R),
		sw_subclass_of(R, ActionC)
	), Rs),
	list_to_set(Rs, RestrictedSet),
	member(RestrictedC, RestrictedSet).
