:- use_module(library('rostest')).
:- use_module(library('lang/query')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('ros/tf/tf')).
:- use_module(library('ros/tf/tf_mongo')).
:- use_module(library('lang/rdf_tests')).

:- use_module('bullettest').

:- begin_rdf_tests(
	   'bullettest',
	   'package://knowrob/owl/test/swrl.owl',
	   [ namespace('http://knowrob.org/kb/swrl_test#'),
		 setup(bullettest_setup),
		 cleanup(bullettest_cleanup)
	   ]).

bullettest_setup :-
	tf_mng_drop,
	tf_logger_enable.


bullettest_cleanup :-
	tf_logger_disable,
	tf_mng_drop.

test('create, show, destroy', [blocked('segfault when there is no user interaction in the window')]) :-
	create_world(World),
	show_world(World),
	sleep(1),
	delete_world(World).

test('create, destroy') :-
	create_world(World),
	delete_world(World).

test('create, add, destroy') :-
	create_world(World),
	add_object(World, box(1,1,1), [[0,0,0], [0,0,0,1]], [mass(1), name(testobject)]),
	delete_world(World).

test('create, add, query, destroy') :-
	create_world(World),
	Pose = [[0.0,0.0,0.0], [0.0,0.0,0.0,1.0]],
	add_object(World, box(1,1,1), Pose, [mass(1), name(testobject)]),
	query_object_pose(World, testobject, QPose),
	assert_unifies(QPose, Pose),
	delete_world(World).

test('create, add2, query2, destroy') :-
	create_world(World),
	Pose1 = [[0.0,0.0,0.0], [0.0,0.0,0.0,1.0]],
	Pose2 = [[2.0,0.0,0.0], [0.0,0.0,0.0,1.0]],
	add_object(World, box(1,1,1), Pose1, [mass(1), name(testobject1)]),
	add_object(World, box(1,1,1), Pose2, [mass(1), name(testobject2)]),
	query_object_pose(World, testobject1, QPose1),
	assert_unifies(QPose1, Pose1),
	query_object_pose(World, testobject2, QPose2),
	assert_unifies(QPose2, Pose2),
	delete_world(World).

:- end_tests('bullettest').
