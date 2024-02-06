:- module(bullet_reasoner, [
			  movement_at_pose(r,+,-),
			  possible_place_pose(r,r,-),
			  object_ends_at_example(-)
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
movement_at_pose(Object, Pose, Distance) :-
	create_world(World),
	show_world(World),
	add_other_objects_to_world(World, Object),
	add_knowrob_object_to_world(World, Object, Pose, [mass(1)]),
	(true; % add delay for presentation
	% Simulate for 3 seconds
	step_world(World,180.0),
	wait_until_finished_simulating(World),
	query_object_pose(World, Object, [EndPos, _]),
	(true;
	delete_world(World),
	Pose = [InitFrame, InitPos, _],
	(  InitFrame == map
	-> CombinedPos = InitPos
	;  tf:tf_get_frame_pose(InitFrame, ['map', MapPos, MapRot]),
	   transform_multiply([map, InitFrame, MapPos, MapRot],
						  [InitFrame, InitFrame, InitPos, [0,0,0,1]],
						  [map, _, CombinedPos, _])
	),
	position_distance(CombinedPos, EndPos, Distance)
	)).

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

/**
 * object_ends_at_example(-Pose) is multi.
 *
 * example usage of the bullet interface.
 * split into 3 steps that can be triggered one after the other via ';' on the command line.
 */
object_ends_at_example(EndPose) :-
	create_world(World),
	StartPose = [[0,0,1.3],[0,0,0,1]],
	(
		(
			% this is here so rosprolog dosn't trigger executing the second step directly after the first.
			% if this was not here, it wouldn't pause right after setting up the world
			0 = 0
		) ; (
			show_world(World),
			add_object(World, box(100, 100, 0.01), [[0,0,-0.01],[0,0,0,1]], [name(ground)]),
			add_object(World, mesh("package://iai_kitchen/meshes/misc/big_table_1.stl"), [[0,0,0.4],[0.05,0,0,1]], [name(table)]),
			add_object(World, mesh("package://iai_kitchen/meshes/misc/bowl.stl"), StartPose, [name(bowl), mass(1)])
		) ; (
			once((repeat,
				  query_object_pose(World, bowl, PoseBefore),
				  step_world(World,60),
				  wait_until_finished_simulating(World),
				  query_object_pose(World, bowl, PoseAfter),
				  PoseBefore == PoseAfter))
		) ; (
			query_object_pose(World, bowl, EndPose),
			delete_world(World)
		)
	).

add_knowrob_object_to_world(World, Object) :-
	add_knowrob_object_to_world(World, Object, []).

add_knowrob_object_to_world(World, Object, Data) :-
	once(object_shape(Object,_,ShapeTerm,Pose,_)),
	add_knowrob_object_to_world0(World, ShapeTerm, Pose, [name(Object)|Data]).

add_knowrob_object_to_world(World, Object, Pose, Data) :-
	once(object_shape(Object,_,ShapeTerm,_,_)),
	add_knowrob_object_to_world0(World, ShapeTerm, Pose, [name(Object)|Data]).

add_knowrob_object_to_world0(World, ShapeTerm, [Frame,FramePos,FrameRot], Data) :-
	% initialize the origin in case it is not set
	ignore([FramePos,FrameRot] = [[0,0,0],[0,0,0,1]]),
	(  Frame == map
	-> [CombinedPos, CombinedRot] = [FramePos, FrameRot]
	;  tf:tf_get_frame_pose(Frame, ['map', Pos, Rot]),
	   transform_multiply([map, Frame, Pos, Rot],
						  [Frame, Frame, FramePos, FrameRot],
						  [map, _, CombinedPos, CombinedRot])
	),
	add_object(World, ShapeTerm, [CombinedPos, CombinedRot], Data).
