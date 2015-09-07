:- module(protelog, []).

:- reexport([ capture_compounds,
              collectors,
              composer
            ]).


tails([], []).
tails(List@[_|Rest], [List|Tails]) :- tails(Rest, Tails).


contrived_call(Pred@p(thing), Arg) :- call(Pred, stuff, Arg).
contrived_call(Pred@q(_,_,_), _)   :- Pred.
contrived_call(Pred, Arg)          :- call(Pred, Arg).
