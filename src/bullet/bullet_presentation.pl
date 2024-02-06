:- module(bullet_presentation, [demonstration/1]).

:- rdf_meta(create_object(r,+,+,-)).
:- rdf_meta(shape_class(+,r)).

demonstration(Distance) :-
	(
		true % pause before start
	);
	(
		tf_logger_enable,
		create_object('http://www.ease-crc.org/ont/SOMA.owl#Floor', [map, [0,0,-0.01],[0,0,0,1]], box(100, 100, 0.01), _),
		create_object('http://www.ease-crc.org/ont/SOMA.owl#Table', [map, [0,0,0.35],[0,0,0,1]], mesh("package://iai_kitchen/meshes/misc/big_table_1.stl"), _),
		create_object('http://www.ease-crc.org/ont/SOMA.owl#Bowl', [map, [0,0,1],[0,0,0,1]], mesh("package://iai_kitchen/meshes/misc/bowl.stl"), _),
		create_object('http://www.ease-crc.org/ont/SOMA.owl#CerealBox', [map, [0,0.15,0.775],[0,0,0,1]], box(0.05, 0.10, 0.15), _),
		marker:republish,
		marker:republish % because unsupported array type '10'
	);
	(
		has_type(Bowl, 'http://www.ease-crc.org/ont/SOMA.owl#Bowl'),
		movement_at_pose(Bowl, [map, [0,0,1],[0,0,0,1]], Distance)
	)
.


create_object(Type, PoseStamped, Shape, Object) :-
	kb_project(
		(
			new_iri(Object, Type),
			has_type(Object, Type)
		)
	),
	universal_scope(Scope),
	tf_set_pose(Object, PoseStamped, Scope),
	assert_shape(Object, Shape).

assert_shape(Object, ShapeTerm) :-
    shape_class(ShapeTerm, Class),
    kb_project(
		(
			new_iri(Shape),
			has_type(Shape, 'http://www.ease-crc.org/ont/SOMA.owl#Shape'),
			new_iri(SR),
			has_type(SR, Class),
			triple(Object,'http://www.ease-crc.org/ont/SOMA.owl#hasShape',Shape),
			triple(Shape,'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#hasRegion',SR)
		)
	),
    assert_shape_region(SR, ShapeTerm),
    !.

% because of problems with the 'soma:' notation, i am using the full URI here.
shape_class(box(_,_,_), 'http://www.ease-crc.org/ont/SOMA.owl#BoxShape'):- !.
shape_class(mesh(_), 'http://www.ease-crc.org/ont/SOMA.owl#MeshShape'):- !.

assert_shape_region(SR, mesh(File)) :-
    kb_project(triple(SR, 'http://www.ease-crc.org/ont/SOMA.owl#hasFilePath', File)),
    !.

assert_shape_region(SR, box(X,Y,Z)) :-
	kb_project((triple(SR, 'http://www.ease-crc.org/ont/SOMA.owl#hasDepth',  X),
		    triple(SR, 'http://www.ease-crc.org/ont/SOMA.owl#hasWidth',  Y),
		    triple(SR, 'http://www.ease-crc.org/ont/SOMA.owl#hasHeight', Z))),
	!.
