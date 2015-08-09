:- module(capture_compounds, [(@)/2]).

% Capture Compounds with using the '@' operator
% 
% Provides syntax akin to Haskell's `@` or MLs `as`
% It allows for capturing lists as a special case:
%
%       ...,
%       term(L@[X|Xs])
%       ...,
%       
% Expands to
%
%       ...,
%       term([X|Xs]),
%       L = [X|Xs],
%       ...,
%
% Can be used to capture any comopound, and in rule heads:
% 
%   term(Capture@compound(With, Args), arg) :-
%       do(With),
%       examine(Args),
%       treat(Capture).
%
% expands to:
%
%   term(compound(With, Args), arg) :-
%       Capture = compound(With, Args),
%       do(With),
%       examine(Args),
%       treat(Capture).
%

:- op(200, xfx, user:(@)).

%% replace(+A, +B, +List, ?Replaced)
%
%  True if Replaced contains all the elements of List, except with
%  elements matching A replaced by B. The match is *not* based on
%  simple unificaiton, beacuse this predicate is designed to allow for
%  uninstantiated variables to be preserved. If C is an element of
%  List, then A matches C iff
%
%    * eiter A == C
%    * or    nonvar(C) and A = C
%
%  This predciate is cabpable of replacing A with uninstantiated parts
%  of A. E.g.,
%
%      A@a => A

replace(_, _, [], []) :- !.
replace(A, B, [C|Cs], [R|Rs]) :-
    copy_term((A, B), (FreshA, FreshB)),
    ( A == C           -> R = C, replace(FreshA, FreshB, Cs, Rs)
    ; nonvar(C), C = A -> R = B, replace(FreshA, FreshB, Cs, Rs)
    ; R = C, replace(A, B, Cs, Rs)
    ).

%% functor_of(+F, +Term)
%
%   True if Term is a compound with F is the functor of Term.

functor_of(F, Term) :- compound(Term), functor(Term, F, _).

%% contains_captureCompouns(+Term)
%
%   True if Term is a compound and contains at least one term
%   with the functur @.

contains_captureCompouns(Term) :-
    compound(Term),
    compound_name_arguments(Term, _, Args),
    member(X, Args),
    functor_of(@, X), !.

%% extract_captureCompounds(+Term, -NewTerm, -Unifications)
%
%   takes a term with capture compounds and returns the term
%   with captures removed and the capture compounds as
%   a conjuction of Capture=Compound pairs.

extract_captureCompounds(Term, NewTerm, Unificaitons) :-
    compound(Term),
    compound_name_arguments(Term, Name, Args),
    bagof(Capture=Compound,
            ( member(Arg, Args),
              functor_of(@, Arg),
              Capture@Compound = Arg ),
            CaptureCompoundsList),
    list_as_tupples(CaptureCompoundsList, Unificaitons),
    replace(_@X, X, Args, NewArgs),
    compound_name_arguments(NewTerm, Name, NewArgs).

%% list_as_tupples(+List, -Tupples)
%
%   Transforms a list, [a,b,c,d] into a tuple (a,b,c,d)

list_as_tupples([X], X) :- !.
list_as_tupples([X|Xs], (X,Rest)) :-
    list_as_tupples(Xs, Rest).


user:term_expansion((Head :- Body), (NewHead :- NewBody)) :-
    contains_captureCompouns(Head),
    extract_captureCompounds(Head, NewHead, CaptureCompounds),
    NewBody = (CaptureCompounds, Body).

user:goal_expansion(Goal, NewGoal) :-
    contains_captureCompouns(Goal),
    extract_captureCompounds(Goal, Term, CaptureCompounds),
    NewGoal = (Term, CaptureCompounds).
