:- module(collectors,
          [ op(550, xfx, user:(are)),
            op(550, xfx, user:(of)),
            op(550, xfx, user:(of_all)),
            op(200, xfx, user:(:<)),
            are/2,
            of/2,
            of_all/2,
            (:<)/2
          ]).

:- meta_predicate are(?,:), of(?,:), of_all(?,:).

E :< S :- member(E, S).

/*
    are/2: sugar for findall/3
*/

Collected are {Collect|Conditions} :-
    findall(Collect, Conditions, Collected).
Collected are (Module:{Collect|Conditions}) :-
    Module:findall(Collect, Conditions, Collected).


/*
    of/2: special aggregation operations on all solutions of a goal
*/

%% Throw a domain error if the left arguemnt to of/2 is an invalid parameter.
%

valid_of_parameter(count(_)). 
valid_of_parameter(sum(_)).
valid_of_parameter(max(_)).
valid_of_parameter(min(_)).
valid_of_parameter(set(_)). 
valid_of_parameter(bag(_)). 
valid_of_parameter(sorted(_)).
valid_of_parameter(n(_,_)).

invalid_of_param_error(InvalidArg) :-
    ValidParams = [count/1, sum/1, max/1, min/1, set/1, bag/1, sorted/1, n/2],
    domain_error('a valid parameter to of/2'-ValidParams, InvalidArg).

%% The left arg to of/2 is invalid if it's a variable
Var of _ :- var(Var), type_error(nonvar, Var).
    
count(Count) of {_Collect|Conditions} :-
    aggregate_all(count, Conditions, Count).
count(Count) of (Module:{_Collect|Conditions}) :-
    Module:aggregate_all(count, Conditions, Count).

sum(Sum) of {Collect|Conditions} :-
    aggregate_all(sum(Collect), Conditions, Sum).
sum(Sum) of (Module:{Collect|Conditions}) :-
    Module:aggregate_all(sum(Collect), Conditions, Sum).

max(Max) of {Collect|Conditions} :-
    aggregate_all(max(Collect), Conditions, Max).
max(Max) of (Module:{Collect|Conditions}) :-
    Module:aggregate_all(max(Collect), Conditions, Max).

min(Min) of {Collect|Conditions} :-
    aggregate_all(min(Collect), Conditions, Min).
min(Min) of (Module:{Collect|Conditions}) :-
    Module:aggregate_all(min(Collect), Conditions, Min).

set(Set) of {Collect|Conditions} :-
    aggregate_all(set(Collect), Conditions, Set).
set(Set) of (Module:{Collect|Conditions}) :-
    Module:aggregate_all(set(Collect), Conditions, Set).

bag(Bag) of {Collect|Conditions} :-
    aggregate_all(bag(Collect), Conditions, Bag).
bag(Bag) of (Module:{Collect|Conditions}) :-
    Module:aggregate_all(bag(Collect), Conditions, Bag).

sorted(Sorted) of {Collect|Conditions} :-
    findall(Collect, Conditions, Results),
    msort(Results, Sorted).
sorted(Sorted) of (Module:{Collect|Conditions}) :-
    Module:findall(Collect, Conditions, Results),
    msort(Results, Sorted).

n(N, Sols) of {Collect|Conditions} :-
    findnsols(N, Collect, Conditions, Sols).
n(N, Sols) of (Module:{Collect|Conditions}) :-
    Module:findnsols(N, Collect, Conditions, Sols).

%% If none of the previous rules to of/2 were tried, then
%% check whether the left argumetn might have been invalid

InvalidArg of _ :-
    \+ valid_of_parameter(InvalidArg),
    invalid_of_param_error(InvalidArg).

/*
    of_all/2: actualizing and checking all solutions of condition
*/

%% Will actualize predications of all solutions in conditions. E.g.:
%
%       length(X,8) of_all X :< [A,B,C,D]
%       
%   Will make each of A, B, C, D an 8 element list.
%
%       (V is  X * X, V < 100) of_all (between(1,9,X), V :< Ns).

Pred of_all Conditions :-
    bagof(Pred, Conditions, Preds),
    maplist(call, Preds).
