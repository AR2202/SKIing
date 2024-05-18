# SKIing
Interpreter for the SKI combinator calculus

https://en.wikipedia.org/wiki/SKI_combinator_calculus

## USAGE
If you have the Haskell tool stack installed, you should be able to use

`stack run`

This will bring up the REPL.

```
Welcome to SKI!
please enter ':q' to quit
or eneter a valid SKI expression
SKI$
```
Enter a SKI expression to reduce it to a simpler SKI expression.

The combinators are applied from left to right. Parentheses are only necessary where required to change the evaluation order, but permitted anywhere.

## Examples

### Identity function

`IK` reuces to `K`

`II` reduces to `I`

`(II)K` reduces to `K` (the parentheses are not required here)

### Representing booleans 
`true` can be represented as `K`

`false` can be represented as `KI` 

`a and b` can then be represented as `a b false ` which corresponds to `a b KI`

Let's validate this representation:

`true and true` should be `true` 

`true and true` => `true true false` => `KK(KI)` => `K` => `true`

`true and false` should be `false` 

`true and false`=>`true false false`=>`K(KI)(KI)` => `KI` => `false`

`false and true` should be `false`

`false and true`=>`false true false`=>`(KI)K(KI)`=>`I(KI)`=>`KI` => `false`

`false and false` should be `false`

`false and false`=> `false false false`=>`KI(KI)(KI)`=> `KI`=> `false`
