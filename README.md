# protelog #

Protelog is an evolving collection of more-and-less experimental
Prolog syntax extensions. The name is meant to evoke the *protean*
nature of the project. Each syntax extension is collected in its own
module which is then reexported by the `protelog` module; thus, users
can sprinkle on the sugar sparingly or mix it in all at once, as they
see fit.

## Current Syntax Extensions ##

### `capture_compounds.pl` ###

Module `capture_compounds.pl` exports a single operator, `@/2`, which
facilitates syntax akin to Haskell's `@` or OCaml's `as`. I.e., it
enables destructuring compound data structures and binding them to a
variable simultaneously.

E.g.,

```prolog
tails([], []).
tails(List@[_|Rest], [List|Tails]) :- tails(Rest, Tails).
```

Which expands to

```prolog
tails([], []).
tails([B|C], [A|D]) :-
    A=[B|C],
    tails(C, D).
```

`@/2` can be used with any terms, not just lists. I think it might
prove useful in meta-programming contexts, when one wants to inspect a
compound term and call the term, but I'm not sure. E.g.,

```prolog
contrived_call(Pred@p(thing), Arg) :- call(Pred, stuff, Arg).
contrived_call(Pred@q(_,_,_), _)   :- Pred.
contrived_call(Pred, Arg)          :- call(Pred, Arg).
```

### `collectors.pl` ###

Module `collectors.pl` aims to provide an alternative syntax for aggregating,
collecting, and testing solutions. It provides 3 operators, `are/2`,
`of/2`, and `of_all/2`. `are/2` is sugar for `findall/3`. `of/2` is
combined with special compounds in its left argument to dispatch calls
to `aggregate_all/3` and `findnsols/3`. `of_all/2` facilitates calling
some goal for all solutions of a condition: it is like a `for_each/2`
that can leave bindings.

Using `are/2` we can rewrite

```prolog
ns(Ns) :-
    findall(X, 
            ( between(1,100,N),
            N^2 > 3,
            X is 2 * N),
            Ns).
```

as

```prolog
ns(Ns) :-
    Ns are { X | between(1, 100, N),
                 N^2 > 3,
                 X is 2 * N }.
```

Using `of/2` we can get pretty, declarative idioms for aggregating results:

```prolog
?- Xs are {X|between(1,10,_), random_between(1,100,X)}.
Xs = [22, 71, 96, 77, 41, 85, 67, 22, 22|...].

?- max(M) of {X | between(1,5,_), random_between(1,100,X)}.
M = 84 

?- set(S) of {X | member(X, [1,1,2,3,3,4,5,5,6,7,78,9])}.
S = [1, 2, 3, 4, 5, 6, 7, 9, 78] 

?- sorted(S) of {X | between(1,10,_), random(X)}.
S = [0.12897679140720977, 0.20889013811096088, 0.2823282255935682, 0.40329472611490347, 0.40330146937321065, 0.6229976729667204, 0.6893687806662105, 0.8018322312531445, 0.8719945135823282|...] 
```

`of_all/2` might be the most powerful of these sugar bits, the virtues of which
might go beyond the aesthetic. I think it might provide a flexible, declarative,
and more general replacement for `maplist`


```prolog
?- length(X,8) of_all member(X, [A,B,C,D,E]).
A = [_G2212, _G2215, _G2218, _G2221, _G2224, _G2227, _G2230, _G2233],
B = [_G2239, _G2242, _G2245, _G2248, _G2251, _G2254, _G2257, _G2260],
C = [_G2266, _G2269, _G2272, _G2275, _G2278, _G2281, _G2284, _G2287],
D = [_G2293, _G2296, _G2299, _G2302, _G2305, _G2308, _G2311, _G2314],
E = [_G2320, _G2323, _G2326, _G2329, _G2332, _G2335, _G2338, _G2341].

?- (X = 2) of_all (length(L, 10), member(X, L)).
L = [2, 2, 2, 2, 2, 2, 2, 2, 2|...].
```

With some refinement and further development, I think the extensions in
`collectors.pl` might be a nice addition to an adventurous Prologers toolbox.

### `composer.pl` ###

Composer is an experiment in LP-friendly, relational programming oriented
compositional syntax. It strives to offer simple syntax sugar for indicating
structures amounting to chains of data transformation. I am not sure that it is
useful, or that it is in any way preferable to the either Michael Hendricks
`func` package % or Carlo Capelli's `lifter` module.

With `composer.pl`, we can rewrite


```prolog
   ?- string_lower("ABCDEFG", Lower), string_chars(Lower, Chars), reverse(Chars, RevChars),
   string_chars(RevChars, LowerCaseReversed).
```

as

```prolog
   ?- LowerCaseReversed =: inv string_chars <- reverse <- string_chars <- string_lower("ABCDEFG").
   LowerCaseReversed = "gfedcba".
```
