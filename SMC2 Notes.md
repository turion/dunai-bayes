# SMC2

* Model `(m theta, MSF (ReaderT theta m) a (b, state))` or `(m theta, MSF m (theta, a) (b, state))`
* Already have `MSF (theta, a, b) state` (Bayes filter)
* Want `MSF (a, b) (state, theta)`
* Be careful that we don't sample `theta` once at the beginning for every particle and then never again:
  This sounds like the collapse Dominic talked about.
  This is also one of the main aims of SMC2 to avoid

## Idea inspired by Dirichlet process

* Two particle filters, one for `state`, one for `theta`
* In the one for `theta`, draw a new value from the prior at every step,
  and add it as a separate particle with weight `1/n` where `n` is the time step
  * This might be beneficial for `state` as well (which can get stuck in a collapse as well if `p(state' | state)` is deterministic for some states)
  * Generalisation (probably for `rhine-bayes`): Weight `alpha/t` where `alpha` is a time constant and `t` is the time passed so far
