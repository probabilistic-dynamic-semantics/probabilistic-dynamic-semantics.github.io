---
sitetitle: probabilistic dynamic semantics
pagetitle: components
---

## Components

- [`Framework`](https://github.com/probabilistic-dynamic-semantics/pds/tree/main/src/Framework):
  - An encoding of Combinatory Categorial Grammar (CCG)-based grammar fragments, as well as a CKY-style parser with string-based (instead of span-based) memoization.
  - A framework for typed λ-calculus based on δ-rules, which transform complex semantic representations into simpler (but equivalent) typed probabilistic models.
  - Code for transforming typed λ-terms into models in the [Stan](https://mc-stan.org/) programming language.
- [`Theory`](https://github.com/probabilistic-dynamic-semantics/pds/tree/main/src/Theory):
  - An encoding of theories of formal pragmatics, including common grounds and Questions Under Discussion (QUDs).
- [`Analysis`](https://github.com/probabilistic-dynamic-semantics/pds/tree/main/src/Analysis):
  - Analyses of particular natural language phenomena, including factivity and vagueness.
