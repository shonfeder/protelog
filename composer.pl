:- module(composer,
          [ op(200, xfx, user:(=:)),
            op(100, xfy, user:(<-)),
            op(50, fx, user:(//)),
            op(50, fx, user:(inv)),
            
            (=:)/2,
            (<-)/2,
            (//)/2,
            inv/2,
            inv/3
          ]).

% Module `composer` is an experiment. It strives to offer simple syntactic sugar for
% indicating structures amounting to chains of data transformation. I am not sure that it is
% useful, or that it is in any way preferable to the either Michael Hendricks `func` package 
% or Carlo Capelli's `lifter` module.

%% ?Defined =: +Predicate
%
% Provide a definition-like syntax for unifying with the last argument of
% a predicate (which is left implicit):
%
%       ?- X =: plus(1,2).
%       X = 3.
%
% When not combined with '<-'/2, it is equivalent to `call(Predicate, Defined)`.
%
% This operator provides syntax that resembles functional notation, but it
% does *not* facilitate functional syntax. It is just notation for emphasizing
% the last component of a relation. E.g.,
%
%       ?- 3 =: plus(1,X).
%       X = 2.

V =: P1 <- P0 :- X =: P0, call(P1, X, V).
V =: Pred     :- Pred \= _ <- _, call(Pred, V).

%% +TruncatedPred1 <- +TruncatedPred0
% 
% Compose predicates on their ultimate and penultimate arguments.
% In the simplest case, `TruncatedPred1 <- TruncatedPred0` It is equivalent to
%
%   call(TruncatedPred0, X), call(TruncatedPred1, X)
%
% With X left implicit. E.g.:
%
%   ?- write <- string_concat("Hello, ", "world!").
%   Hello, world!
%
% Used with =:/2, these compositions can be chained to obtain a result on the lhs,
% with the intermediate arguments left implicit. It lets one eliminate redundant veriables
% when one's only dealing with a simple flow of data or series of transformations. E.g., we
% can replace this
%
%   ?- string_lower("ABCDEFG", Lower), string_chars(Lower, Chars), reverse(Chars, RevChars),
%   string_chars(RevChars, LowerCaseReversed).
%
% with the following:
%
%   ?- LowerCaseReversed =: inv string_chars <- reverse <- string_chars <- string_lower("ABCDEFG").
%   LowerCaseReversed = "gfedcba".

P2 <- P1 <- P0 :- X =: P0, P1_ =.. [P1,X], P2 <- P1_.
P1 <- P0 :- X =: P0, X =: P1.

%% //(+Pred, ?Var, ?Var)
%% //(?Var, ?Var)
%
% "Pass through" arguments.
%
% Useful for testing an argument's properties, e.g.:
%
%   ?-  X =: sumlist <- //is_list <- last([1,2,3,[4,5,6]]).
%   X = 15.
%
% Also useful for passing along bound partial terms, e.g.:
%
%   ?- TenThrees =: //maplist(=(3)) <- inv length(10).
%   TenThrees = [3, 3, 3, 3, 3, 3, 3, 3, 3|...].
%
% Finally, it can be used to emphasize the initial argument
% fed into the chain of predicates:
%
%   X =: maplist(inv string_chars) <- maplist(reverse) <- maplist(string_chars) <- //["abc",
%   "123", "xyz"].
%   X = ["cba", "321", "zyx"] 

//(P1, X, X) :- call(P1, X).
//(X, X).

%% inv(+Pred, ?A, ?B)
%
% The inverse of a relation Pred, e.g.,
%
%   ?- length(X, 3).
%   X = [_G32877, _G32880, _G32883].
%
%   ?- inv(length, 3, X).
%   X = [_G32873, _G32876, _G32879].
%
% inv/2 is sugar to allow for cleaner inversions in compositions:
%
%   ?- FiveThrees =: maplist(plus(1)) <- //maplist(=(2)) <- inv length(5).
%   FiveThrees = [3, 3, 3, 3, 3] 

inv(P, A, B) :- call(P, B, A).
inv(P0, B)    :-
    P0 =.. [F|Args],
    append(Rest, [A], Args),
    append(Rest, [B, A], InvertedArgs),
    apply(F, InvertedArgs).
