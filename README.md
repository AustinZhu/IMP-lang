# IMP-lang

![](https://travis-ci.org/AustinZhu/IMP-Parser.svg?branch=master)

A implementation of the IMP language from *"The Formal Semantics of Programming Languages"*

## Syntax

*a* ::= *n*
    | *X*
    | *a* + *a*
    | *a* - *a*
    | *a* × *a*
  
*b* ::= true
    | false
    | *a* = *a*
    | *a* ≤ *a*
    | ¬ *b*
    | *b* ∧ *b*
    | *b* ∨ *b*


*c* ::= skip
    | *X* := *a*
    | *c*; *c*
    | if *b* then *c* else *c*
    | while *b* do *c*

## Evaluation

- Aexp

  <*n*, *σ*> → *n*
  
  <*X*, *σ*> → *σ*(*X*)

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* + *a1*, *σ*> → *n0* + *n1*

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* - *a1*, *σ*> → *n0* - *n1*

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* × *a1*, *σ*> → *n0* × *n1*
