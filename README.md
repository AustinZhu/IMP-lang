# IMP-lang

[![Haskell CI](https://github.com/AustinZhu/IMP-lang/actions/workflows/haskell.yml/badge.svg)](https://github.com/AustinZhu/IMP-lang/actions/workflows/haskell.yml)
[![Docker](https://github.com/AustinZhu/IMP-lang/actions/workflows/docker.yml/badge.svg)](https://github.com/AustinZhu/IMP-lang/actions/workflows/docker.yml)

![image](https://user-images.githubusercontent.com/42071208/140778328-befad51a-f9c2-44ab-b334-64ffd8bfac4a.png)

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

  <*a₀*, *σ*> → *n₀*, <*a₁*, *σ*> → *n₁* ⊢ <*a₀* + *a₁*, *σ*> → *n* (if *n* = *n₀* + *n₁*)

  <*a₀*, *σ*> → *n₀*, <*a₁*, *σ*> → *n₁* ⊢ <*a₀* - *a₁*, *σ*> → *n* (if *n* = *n₀* - *n₁*)

  <*a₀*, *σ*> → *n₀*, <*a₁*, *σ*> → *n₁* ⊢ <*a₀* × *a₁*, *σ*> → *n* (if *n* = *n₀* × *n₁*)

- Bexp

  ⊢ <true, *σ*> → true
  
  ⊢ <false, *σ*> → false
  
  <*a₀*, *σ*> → *n*, <*a₁*, *σ*> → *m* ⊢ <*a₀* = *a₁*, *σ*> → true (if *n* ≡ *m*)
  
  <*a₀*, *σ*> → *n*, <*a₁*, *σ*> → *m* ⊢ <*a₀* = *a₁*, *σ*> → false (if *n* ≠ *m*)
  
  <*a₀*, *σ*> → *n*, <*a₁*, *σ*> → *m* ⊢ <*a₀* ≤ *a₁*, *σ*> → true (if *n* ≤ *m*)
  
  <*a₀*, *σ*> → *n*, <*a₁*, *σ*> → *m* ⊢ <*a₀* ≤ *a₁*, *σ*> → false (if *n* > *m*)
  
  <*b*, *σ*> → true ⊢ <¬*b*, *σ*> → false
  
  <*b*, *σ*> → false ⊢ <¬*b*, *σ*> → true
  
  <*b₀*, *σ*> → *t₀*, <*b₁*, *σ*> → *t₁* ⊢ <*b₀* ∧ *b₁*, *σ*> → *t* (*t* = true if *t₀* ≡ true and *t₁* ≡ true, otherwise *t* = false)
  
  <*b₀*, *σ*> → *t₀*, <*b₁*, *σ*> → *t₁* ⊢ <*b₀* ∨ *b₁*, *σ*> → *t* (*t* = true if *t₀* ≡ true or *t₁* ≡ true, otherwise *t* = false)
  
- Com
  
  ⊢ <skip, *σ*> → *σ*
  
  <*a*, *σ*> → *m* ⊢ <*X* := *a*, *σ*> → *σ*\[*m*/*X*\]
  
  <*c₀*, *σ*> → *σ''*, <*c₁*, *σ''*> → *σ'* ⊢ <*c₀*;*c₁*, *σ*> → *σ'*
  
  <*b*, *σ*> → true, <*c₀*, *σ*> → *σ'* ⊢ <if *b* then *c₀* else *c₁*, *σ*> → *σ'*
  
  <*b*, *σ*> → false, <*c₁*, *σ*> → *σ'* ⊢ <if *b* then *c₀* else *c₁*, *σ*> → *σ'*
  
  <*b*, *σ*> → false ⊢ <while *b* do *c*, *σ*> → *σ*
  
  <*b*, *σ*> → true, <*c*, *σ*> → *σ''*, <while *b* do *c*, *σ''*> → *σ'* ⊢ <while *b* do *c*, *σ*> → *σ'*
