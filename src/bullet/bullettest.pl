:- module(bullettest, []).
/**
module to test bulletworld stuff
*/

:- use_foreign_library('libkb_bullettest.so').

/**
 * add_object(+World, +Object, +Pose) is det
 *
 * add_object/3 adds an object to the world without any extra data
 *
 * @see add_object/4 for specifing extra data like mass for the object and for supported objects
 */
add_object(World, Object, Pose) :-
    add_object(World, Object, Pose, []).

/**
 * add_object(+World, +Object, +Pose, +Data) is det
 *
 * add_object/4 adds an object to the world at the specified Pose with the specified Data
 *
 * @arg World The world id, as returned by create_world/1
 * @arg Object The object to add, must bo one of the following:
 * * box(X,Y,Z)
 *   a simple box
 * * mesh(Filename)
 *   a file with a mesh in a format that assimp can parse
 * @arg Pose The pose of the Object, in the format [Pos, Rotation] where Pos is [X,Y,Z] and Rotation is [X,Y,Z,W]
 * @arg Data a list with additional Data for the object. Currently Supported data:
 * * mass(Mass)
 *   The mass is a float. A mass of 0 means that the object can't move.
 * * friction(Friction)
 * * name(Name)
 *   The name of the object as an atom.
 */

/**
 * query_object_pose(+World, +Object, -Pose) is semidet.
 *
 * query where an object is in a world
 *
 * @arg World The world id.
 * @arg Object The object name.
 * @arg Pose the pose in the format [Position, Rotation].
 */

/**
 * bullettest is nondet
 *
 * bullettest/0 is a temporary predicate used to test and debug the bullet world while developing it.
 */
bullettest(Object,Part) :-
    create_world(World),
    show_world(World),
    !,
    is_physical_object(A),
    object_shape(A,_,C,[Frame,FramePos,FrameRot],_),
    tf:tf_get_frame_pose(Frame, ['map', Pos, Rot]),
    transform_multiply([map, Frame, Pos, Rot],
		       [Frame, Frame, FramePos, FrameRot],
		       [map, _, CombinedPos, CombinedRot]),
    add_object(World, C, [CombinedPos, CombinedRot], [name(A)]),
    Object = A,
    Part = C.
