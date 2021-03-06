:- module(capture_compounds, [(@)/2]).

% Capture Compounds using the '@/2' operator
% 
% module `capture_compounds` provides syntax akin to Haskell's `@` or MLs `as`
% It allows for capturing lists as a special case. E.g.,
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
% @/2 nan be used to capture any comopound.
%
% Term expansion is used to extract patterns using @/2 in rule heads as well:
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
%   True if Term is a compound and F is its functor.

functor_of(F, Term) :- compound(Term), functor(Term, F, _).

%% contains_captureCompounds(+Term)
%
%   True if Term is a compound that contains at least one term
%   with the functur @.

contains_captureCompounds(Term) :-
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


/*
    TERM EXPANSIONS
*/

%% Expand capture_compounds in rule heads.
%

user:term_expansion((Head :- Body), (NewHead :- NewBody)) :-
    contains_captureCompounds(Head),
    extract_captureCompounds(Head, NewHead, CaptureCompounds),
    NewBody = (CaptureCompounds, Body).

%% Expand capture_compounds in facts.
%

user:term_expansion(Fact, (Head :- Body)) :-
    Fact \= (_ :- _),
    contains_captureCompounds(Fact),
    extract_captureCompounds(Fact, Head, CaptureCompounds),
    Body = CaptureCompounds.

%% Expand capture_compounds in goals.
%

user:goal_expansion(Goal, NewGoal) :-
    contains_captureCompounds(Goal),
    extract_captureCompounds(Goal, Term, CaptureCompounds),
    NewGoal = (Term, CaptureCompounds).

