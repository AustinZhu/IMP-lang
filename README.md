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
    | ¬*b*
    | *b* ∧ *b*
    | *b* ∨ *b*


*c* ::= skip
    | *X* := *a*
    | *c*; *c*
    | if *b* then *c* else *c*
    | while *b* do *c*

## Evaluation

- Aexp

  ⊢ <*n*, *σ*> → *n*
  
  ⊢ <*X*, *σ*> → *σ*(*X*)

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* + *a1*, *σ*> → *n* (if *n* = *n0* + *n1* )

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* - *a1*, *σ*> → *n* (if *n* = *n0* - *n1* )

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* × *a1*, *σ*> → *n* (if *n* = *n0* × *n1* )

- Bexp

  ⊢ <true, *σ*> → true
  
  ⊢ <false, *σ*> → false
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* = *a1*, *σ*> → true (if *n* ≡ *m* )
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* = *a1*, *σ*> → false (if *n* ≠ *m* )
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* ≤ *a1*, *σ*> → true (if *n* ≤ *m* )
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* ≤ *a1*, *σ*> → false (if *n* > *m* )
  
  <*b*, *σ*> → true ⊢ <¬*b*, *σ*> → false
  
  <*b*, *σ*> → false ⊢ <¬*b*, *σ*> → true
  
  <*b0*, *σ*> → *t0*, <*b1*, *σ*> → *t1* ⊢ <*b0* ∧ *b1*, *σ*> → *t* (*t* = true if *t0* ≡ *true* and *t1* ≡ true, otherwise *t* = false)
  
  <*b0*, *σ*> → *t0*, <*b1*, *σ*> → *t1* ⊢ <*b0* ∨ *b1*, *σ*> → *t* (*t* = true if *t0* ≡ *true* or *t1* ≡ true, otherwise *t* = false)
  
- Com
  
  ⊢ <skip, *σ*> → *σ*
  
  
