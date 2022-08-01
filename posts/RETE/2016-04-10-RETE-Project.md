---
title: RETE Project Part 1
author: tomberek
tags: RETE, OPS5, rules, rewriting
toc: yes
---

# Introduction

This sequence will look at the implementation of the [RETE algorithm](https://en.wikipedia.org/wiki/Rete_algorithm). RETE is a pattern matching algorithm that is optimized by reusing partial matches. The motivation for this project is to bring optimized rule matching to Haskell.

# The Plan
(will be edited throughout project)

## OPS5
Implement an OPS5 DSL. A good [manual](http://www.cs.gordon.edu/local/courses/cs323/OPS5/ops5.html). Currently learning and using [compdata](https://hackage.haskell.org/package/compdata-0.10) - [paper](http://www.diku.dk/~paba/pubs/files/bahr11wgp-paper.pdf). Perhaps [compdata-param](https://hackage.haskell.org/package/compdata-param) - [paper](http://arxiv.org/pdf/1202.2917.pdf).

[Another Summary](http://www.drdobbs.com/architecture-and-design/the-rete-matching-algorithm/184405218)

Should we just use a [BNF converter](http://bnfc.digitalgrammars.com/)? Almost [a BNF expression](https://gist.github.com/krodelin/9853a5bbfcac76592b2a).

## Rules
Use [ho-rewriting](https://hackage.haskell.org/package/ho-rewriting-0.2) - [paper](http://www.cse.chalmers.se/~emax/documents/axelsson2015lightweight.pdf) or [Yicho](http://takeichi.ipl-lab.org/yicho/)

## RETE
There are various implementations that may be of interest; [RETE](http://www.csl.sri.com/users/mwfong/Technical/RETE%20Match%20Algorithm%20-%20Forgy%20OCR.pdf) and [LEAPS](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.96.5371&rep=rep1&type=pdf).

## LEAPS

## Another?
[Venus](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.56.6958&rep=rep1&type=pdf)

## Haskell Rules Framework
``` haskell
main :: IO ()
main = print "hi"
```

## ???

## Profit
