---
sitetitle: probabilistic dynamic semantics
pagetitle: quick start
---

## Quick start

There is a test-suite under active development [here](https://github.com/probabilistic-dynamic-semantics/pds/blob/readme/test/Spec.hs).
To load the examples which are currently implemented in [`Analysis`](https://github.com/probabilistic-dynamic-semantics/pds/tree/readme/src/Analysis), install the library and load the test-suite:

```
ghci> :l test/Spec.hs
```

There are three models in the test-suite which can be printed out.
The model on factivity can be gotten by doing:

```
stanOutput factivityExample
```

It yields the following result:

```
parameters {
  real<lower=0.0, upper=1.0> v;
  real<lower=0.0, upper=1.0> w;
  real<lower=0.0, upper=1.0> u;
}

model {
  v ~ logit_normal(0.0, 1.0);
  w ~ logit_normal(0.0, 1.0);

  target += log_mix(v, truncated_normal_lpdf(y | 1.0, u, 0.0, 1.0), truncated_normal_lpdf(y | w, u, 0.0, 1.0));
}
```

The other two models are `scaleNormingExample` and `likelihoodExample`.
