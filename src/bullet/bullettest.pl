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
 */
