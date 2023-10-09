:- use_module(library('rostest')).
:- use_module(library('lang/query')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('ros/tf/tf')).
:- use_module(library('ros/tf/tf_mongo')).
:- use_module(library('lang/rdf_tests')).

:- use_module('bullet_reasoner').

:- begin_rdf_tests(
	   'bullet_reasoner',
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

test('movement_at_position fall') :-
	kb_project(
		% First the box
		( new_iri(Box, soma:'CerealBox'),
		  has_type(Box, soma:'CerealBox'),
		  new_iri(Shape, soma:'Shape'),
		  triple(Box,soma:hasShape,Shape),
		  new_iri(SR, soma:'BoxShape'),
		  triple(Shape,dul:hasRegion,SR),
		  triple(SR, soma:hasDepth,  0.2),
		  triple(SR, soma:hasWidth,  0.1),
		  triple(SR, soma:hasHeight, 0.25),
		  new_iri(Origin, knowrob:'Pose')
		  triple(SR,'http://knowrob.org/kb/urdf.owl#hasOrigin',Origin),
		  triple(Origin, soma:hasPositionVector, [0,0,1]),
		  triple(Origin, soma:hasOrientationVector, [0,0,0,1]),
		  % Then the furniture (table, but there is no table class defined)
		  new_iri(Box, soma:'DesignedFurniture'),
		  has_type(Box, soma:'DesignedFurniture'),
		  new_iri(Shape, soma:'Shape'),
		  triple(Box,soma:hasShape,Shape),
		  new_iri(SR, soma:'BoxShape'),
		  triple(Shape,dul:hasRegion,SR),
		  triple(SR, soma:hasDepth,  0.5),
		  triple(SR, soma:hasWidth,  1.0),
		  triple(SR, soma:hasHeight, 0.25),
		  new_iri(Origin, knowrob:'Pose')
		  triple(SR,'http://knowrob.org/kb/urdf.owl#hasOrigin',Origin),
		  triple(Origin, soma:hasPositionVector, [0,0,0]),
		  triple(Origin, soma:hasOrientationVector, [0,0,0,1])
		)),
	fail. %TODO
