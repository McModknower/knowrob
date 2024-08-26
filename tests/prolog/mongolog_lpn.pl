
% define lpn:jealous as a computable predicate.
% @see https://lpn.swi-prolog.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse1
'http://knowrob.org/kb/lpn#jealous'(X,Y) :-
	triple(X, lpn:loves, Z),
	triple(Y, lpn:loves, Z),
	X \== Y.
