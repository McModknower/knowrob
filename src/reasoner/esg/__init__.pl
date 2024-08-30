
:- sw_register_prefix(dul,  'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#').
:- sw_register_prefix(soma, 'http://www.ease-crc.org/ont/SOMA.owl#').
:- sw_register_prefix(knowrob, 'http://knowrob.org/kb/knowrob.owl#').

:- use_module('esg').

:- use_module('interval').
:- use_module('workflow').
:- sw_register_computable(soma:simultaneous,	interval_equals).
:- sw_register_computable(soma:before,			interval_before).
:- sw_register_computable(soma:after,			interval_after).
:- sw_register_computable(soma:meets,			interval_meets).
:- sw_register_computable(soma:metBy,			interval_met_by).
:- sw_register_computable(soma:starts,			interval_starts).
:- sw_register_computable(soma:startedBy,		interval_started_by).
:- sw_register_computable(soma:finishes,		interval_finishes).
:- sw_register_computable(soma:overlappedOn,	interval_overlaps).
:- sw_register_computable(soma:overlappedBy,	interval_overlapped_by).
:- sw_register_computable(soma:during,			interval_during).

:- use_module('parser').
