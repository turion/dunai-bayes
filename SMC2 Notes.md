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

### Alternative

* Number of particles increases with time
  * #P = floor(log_a(n_0 + n)) particles at every time step
  * a and n_0 are inference-hyperparameters
    * a = how much processable information we are expecting to arrive per time step
    * n_0 = how many equivalent measurements we assume to be encoded in the prior
    * log_a(n_0) = number of initial particles
    * Alternative parametrization
      * #P_0 = number of initial particles
      * #P_max = upper bound on number of particles the machine can support
      * N_max = upper bound on number of expected measurements
      * #P_max = log_a(n_0 + N_max) => a^#P_max = n_0 + N_max => n_0 = a^#P_max - N_max
      * #P_0 = log_a(n_0) = ln(n_0) / ln(a) => a = exp(ln(n_0)/#P_0) = n_0^{1/#P_0}
      * #P_max / #P_0 = ln(n_0 + N_max) / ln(n_0) ~= ln(N_max) / ln(n_0) => n_0 ~= N_max^{#P_0/#P_max} => a = N_max^{1/#P_max}
* Every time #P increases, add a particle from the prior
* Can calculate #P as sum over 1/((n_0 + n) * ln(a))
  * This shows the connection to Dirichlet with alpha = 1/ln(a)
* Collapse of static parameters and states gets unstuck when a new particle is added
  * A new particle becomes rarer and rarer, but never stops
  * But the number of total particles also increases, so collapse also becomes rarer
