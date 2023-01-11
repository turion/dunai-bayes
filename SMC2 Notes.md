# SMC2

* Model `(m theta, MSF (ReaderT theta m) a (b, state))` or `(m theta, MSF m (theta, a) (b, state))`
* Already have `MSF (theta, a, b) state` (Bayes filter)
* Want `MSF (a, b) (state, theta)`
* Be careful that we don't sample `theta` once at the beginning for every particle and then never again:
  This sounds like the collapse Dominic talked about.
  This is also one of the main aims of SMC2 to avoid
