# IMP-lang

[![Haskell CI](https://github.com/AustinZhu/IMP-lang/actions/workflows/haskell.yml/badge.svg)](https://github.com/AustinZhu/IMP-lang/actions/workflows/haskell.yml)

An implementation of the IMP language from *"The Formal Semantics of Programming Languages"* with a REPL environment.

## Introduction

IMP is a small language of while programs.
IMP is called an "imperative" language because program execution involves carrying out a series of explicit commands to change state.
Formally, IMP's behaviour is described by rules which specify how its expressions are evaluated and its commands are executed.

## Usage

Currently, there is no pre-build binaries.

To build and run the REPL, execute:

`stack build --exec IMP-lang-exe`

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
    | *c*;*c*
    | if *b* then *c* else *c*
    | while *b* do *c*

## Evaluation Rules

- Aexp

  ⊢ <*n*, *σ*> → *n*
  
  ⊢ <*X*, *σ*> → *σ*(*X*)

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* + *a1*, *σ*> → *n* (if *n* = *n0* + *n1*)

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* - *a1*, *σ*> → *n* (if *n* = *n0* - *n1*)

  <*a0*, *σ*> → *n0*, <*a1*, *σ*> → *n1* ⊢ <*a0* × *a1*, *σ*> → *n* (if *n* = *n0* × *n1*)

- Bexp

  ⊢ <true, *σ*> → true
  
  ⊢ <false, *σ*> → false
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* = *a1*, *σ*> → true (if *n* ≡ *m*)
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* = *a1*, *σ*> → false (if *n* ≠ *m*)
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* ≤ *a1*, *σ*> → true (if *n* ≤ *m*)
  
  <*a0*, *σ*> → *n*, <*a1*, *σ*> → *m* ⊢ <*a0* ≤ *a1*, *σ*> → false (if *n* > *m*)
  
  <*b*, *σ*> → true ⊢ <¬*b*, *σ*> → false
  
  <*b*, *σ*> → false ⊢ <¬*b*, *σ*> → true
  
  <*b0*, *σ*> → *t0*, <*b1*, *σ*> → *t1* ⊢ <*b0* ∧ *b1*, *σ*> → *t* (*t* = true if *t0* ≡ *true* and *t1* ≡ true, otherwise *t* = false)
  
  <*b0*, *σ*> → *t0*, <*b1*, *σ*> → *t1* ⊢ <*b0* ∨ *b1*, *σ*> → *t* (*t* = true if *t0* ≡ *true* or *t1* ≡ true, otherwise *t* = false)
  
- Com
  
  ⊢ <skip, *σ*> → *σ*
  
  <*a*, *σ*> → *m* ⊢ <*X* := *a*, *σ*> → *σ*\[*m*/*X*\]
  
  <*c0*, *σ*> → *σ''*, <*c1*, *σ''*> → *σ'* ⊢ <*c0*;*c1*, *σ*> → *σ'*
  
  <*b*, *σ*> → true, <*c0*, *σ*> → *σ'* ⊢ <if *b* then *c0* else *c1*, *σ*> → *σ'*
  
  <*b*, *σ*> → false, <*c1*, *σ*> → *σ'* ⊢ <if *b* then *c0* else *c1*, *σ*> → *σ'*
  
  <*b*, *σ*> → false ⊢ <while *b* do *c*, *σ*> → *σ*
  
  <*b*, *σ*> → true, <*c*, *σ*> → *σ''*, <while *b* do *c*, *σ''*> → *σ'* ⊢ <while *b* do *c*, *σ*> → *σ'*
