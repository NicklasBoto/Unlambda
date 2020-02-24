# Unλαmβdα
## Almost good Unlambda interpreter, written in Haskell

**Unlambda: Your Functional Programming Language Nightmares Come True.**
Unlambda is an esoteric programming language, written by David Madore. It is based on the SKI-combinator calculus, a version of the lambda calculus. The syntax consists of the operators S and K (and also I, but it can be written with S and K).

```
S := λf.λg.λx.f x (g x)
K := λx.λy.x
I := λx.x => λx.skkx
```

In Unlambda these are written with _s_, _k_, _i_, and application is written with _\`_. This version of the language also supports the "syntactic sugar" operators _.x_, _v_, and _r_ (which just prints newline).

### Cheat Sheet
```
k (“constant generator”)
    The k function takes an argument X and returns the function `kX (see below).

`kX (“constant function”)
    The `kX function (which is not primitive but obtained by applying the primitive function k to some function X) takes an argument, ignores it and returns X.

s (“substitution”)
    The s function takes an argument X and returns the function `sX (see below).

`sX (“substitution first partial”)
    The `sX function (which is not primitive but obtained by applying the primitive function s to some function X) takes an argument Y and returns the function ``sXY (see below).

``sXY (“substituted application”)
    The ``sXY function (which is not primitive but obtained by applying the primitive function s to two functions X and Y successively) takes an argument Z and returns the evaluation of ``XZ`YZ.

i (“identity”)
    The i function takes an argument and returns that argument.

v (“void”)
    The v function takes an argument X and returns v itself.
    
.x (“print”) and r (“carriage return”)
    The .x function is written using two characters. The first character is a period and the second is any character. Nevertheless, .x is a single function in Unlambda, and x in this expression is merely a character (read during parsing), not a parameter to the function. The r function is exactly equivalent to .(newline). The .x function behaves like the i (identity) function, with the side effect that it prints the character x (to the standard output) when it is applied. The r function also behaves like the identity and prints a newline character.
```

From David Madore's original page: <http://www.madore.org/~david/programs/unlambda/>.
