:- module(spatial_distance,
    [ object_distance(r,r,?),
      position_distance(+,+,?)
    ]).

% TODO: consider qualitative distance: close-to, far-away-from, ...

%% object_distance(+A:iri, +B:iri, ?Distance:float) is semidet
% 
% Computes euclidean distance between A and B.
%
% @param A         Instance of SpatialThing
% @param B         Instance of SpatialThing
% @param Distance  The current distance between A and B
%
object_distance(A,B,Distance) :-
	ground(A),
	ground(B),
	% FIXME: hardcoded map
	is_at(A, [map,PosA,_]),
	is_at(B, [map,PosB,_]),
	position_distance(PosA, PosB, Distance).

%% position_distance(+A:Pos, +B:Pos, ?Distance:float) is det.
%
% Computes euclidean distance between A and B.
%
% @param A         list of 3 floats
% @param B         list of 3 floats
% @param Distance  The current distance between A and B
position_distance([AX,AY,AZ],[BX,BY,BZ],Distance) :-
	DX is AX - BX,
	DY is AY - BY,
	DZ is AZ - BZ,
	Distance is sqrt(((DX*DX) + (DY*DY)) + (DZ*DZ)).
