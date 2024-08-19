\page prolog Prolog

Prolog-based inference integrates nicely in KnowRob query evaluation.
The data structures used by KnowRob can easily be mapped to Prolog terms and ice versa.

Prolog uses its owl in-memory triple store, and the common way would be to load triples from the KnowRob triple store into Prolog
such that Prolog can reason over them.
However, Prolog can also interact with other data backends in theory, but due to the way how query evaluation works via backtracking,
the overhead of performing evaluation with external data backends can be high.

### Linking Prolog Rules to KnowRob

To use the `PrologReasoner` with your own rules, you need to create a Prolog file that defines the rules and register them in the configuration file.
An example of such a file is shown below.

```json
{
  "semantic-web": {
    "prefixes": [
      { "alias": "lpn", "uri":  "http://knowrob.org/kb/lpn" }
    ]
  },
  "data-sources": [
    {
      "path": "lpn.owl",
      "format": "turtle"
    }
  ],
  "data-backends": [
    {
      "name": "pl",
      "type": "Prolog:rdf_db"
    }
  ],
  "reasoner": [
    {
      "name": "pl",
      "type": "Prolog",
      "data-backend": "pl",
      "imports": [
        {
          "path": "lpn.pl",
          "format": "prolog"
        }
      ]
    }
  ]
}
```

It configures the `PrologReasoner` to use the `Prolog:rdf_db` backend and to import the `lpn.pl` and `lpn.owl` files.

Prolog rules can then be linked to KnowRob through *RDF predicates*.
The following example shows how to define a Prolog rule in a file `lpn.pl` and link it to KnowRob.

```prolog
:- use_module(library('semweb'), [ sw_register_computable/1,
                                   sw_register_prefix/2 ]).

% register the 'lpn' namespace and the computable relation defined in this file.
:- sw_register_prefix(lpn, 'http://knowrob.org/kb/lpn#').
:- sw_register_computable(lpn:jealous).

% define lpn:jealous as a computable predicate.
% @see https://lpn.swi-prolog.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse1
'http://knowrob.org/kb/lpn#jealous'(X,Y) :-
	rdf_has(X, lpn:loves, Z),
	rdf_has(Y, lpn:loves, Z),
	X \== Y.

```

It defines a rule that states that two persons are jealous of each other if they both love the same person.
The rule is registered as a computable predicate `lpn:jealous` and can be used in KnowRob queries.
