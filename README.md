# protelog #

Protelog is an evolving collection of more-and-less experimental
Prolog syntax extensions. The name is meant to evoke the *protean*
nature of the project. Each syntax extension is collected in its own
module which is then reexported by the `protelog` module; thus, users
can sprinkle on the sugar sparingly or mix it in all at once, as they
see fit.

## Current Syntax Extensions ##

### capture_compounds ###

Module `capture_compounds` exports a single operator, `@/2`, which
facilitates syntax akin to Haskell's `@` OCaml's `as`. I.e., it
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
