:- module(bullet_reasoner, [
			  movement_at_pose(r,+,-),
			  possible_place_pose(r,r,-)
		  ]).

:- use_module(bullettest).
:- use_module(library('reasoning/spatial/distance'), [position_distance/3]).

/**
 * movement_at_position(+Object, +Pose, -Distance) is det.
 *
 * Simulate placing the object at Pose and
 * record how far from there it would move.
 *
 * @arg Object The object IRI.
 * @arg Pose The pose to place it at.
 * @arg Distance The euclidian distance the object moved in the simulation.
 */
movement_at_position(Object, Pose, Distance) :-
	create_world(World),
	show_world(World),
	add_other_objects_to_world(World, Object),
	add_knowrob_object_to_world(World, Object, Pose, [mass(1)]),
	% Simulate for 10 seconds
	step_world(World,600.0),
	wait_until_finished_simulating(World),
	query_object_pose(World, Object, [EndPos, _]),
	delete_world(World),
	Pose = [InitFrame, InitPos, _],
	tf:tf_get_frame_pose(InitFrame, ['map', MapPos, MapRot]),
	transform_multiply([map, InitFrame, Pos, Rot],
					   [InitFrame, InitFrame, InitPos, [0,0,0,1]],
					   [map, _, CombinedPos, _]),
	position_distance(CombinedPos, EndPos, Distance).

/**
 * possible_place_pose(+Object, +Surface, -Pose) is det.
 *
 * use movement_at_position/3 to get a possible place position for the object on the surface.
 *
 * @arg Object The object IRI that should be placed.
 * @arg Surface The object IRI of the surface that the object will be placed on.
 * @arg Pose The pose the object should be placed at.
 */
possible_place_pose(Object, Surface, Pose) :-
	fail. % TODO

add_other_objects_to_world(World, Object) :-
	forall(
		(is_physical_object(OtherObject),
		 \+ Object == OtherObject),
		add_knowrob_object_to_world(World, OtherObject)
	).

add_knowrob_object_to_world(World, Object, Data) :-
	object_shape(Object,_,ShapeTerm,Pose,_),
	add_knowrob_object_to_world0(World, ShapeTerm, Pose, [name(Object)|Data]).

add_knowrob_object_to_world(World, Object, Pose, Data) :-
	object_shape(Object,_,ShapeTerm,_,_),
	add_knowrob_object_to_world0(World, ShapeTerm, Pose, [name(Object)|Data]).

add_knowrob_object_to_world0(World, ShapeTerm, [Frame,FramePos,FrameRot], Data) :-
	tf:tf_get_frame_pose(Frame, ['map', Pos, Rot]),
	transform_multiply([map, Frame, Pos, Rot],
					   [Frame, Frame, FramePos, FrameRot],
					   [map, _, CombinedPos, CombinedRot]),
	add_object(World, ShapeTerm, [CombinedPos, CombinedRot], Data).
