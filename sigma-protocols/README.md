## Sigma Protocols

### References

- [On Sigma Protocols](http://cs.au.dk/~ivan/Sigma.pdf)
- [Sigma Protocols (slides)](https://www.math.leidenuniv.nl/~edix/oww/mathofcrypt/schoenmakers/sigma-Leiden2003.pdf)
- [Lecture notes from ECE498AC](http://soc1024.ece.illinois.edu/teaching/ece498ac/fall2018/)

### Definitions

(Damgard 2)

Sigma protocol defined on a binary relation `R` on `{0,1}* x {0,1}*` (pairs of bitstrings) where if `(x, w) <- R` then `len(w)` is `p(|x|)` (polynomial in the length of x)

`x` is an instance of some computational problem, and `w` is a solution to that instance ("witness")

Example relations
- Discrete log - `{(x, w) | x = (p, q, g, h), |g| = |h| = q, h = g^w}`
  with `p`, `q` prime, `g, h <- Z_p*`, `w <- Z_q`
- Random oracle - `{(x, w) | x = hash(w) }`
  (where `w` length restricted appropriately)

Sigma protocol `P` for a relation `R`:
- `x` is known to `Pr`, `Vr`, `w` with `(x, w) <- R` is known to `Pr`
- `P` is of three-move form:
  - `Pr` sends message `a`
  - `Vr` sends `t`-bit string `e` ("challenge")
  - `Pr` sends reply `z`, `Vr` decides to accept/reject based on `x, a, e, z`.

Must have three properties:
1. *Completeness*: if `Pr`, `Vr` follow protocol, `Vr` accepts with `p = 1`
2. *Special soundness*: for any `x` and any pair of conversations `(a, e, z)`, `(a, e', z')` with `e /= e'`,
  `w` s.t. `(x, w) <- R` can be efficiently computed
3. *Special honest-verifier ZK*: there exists poly-time simulator `M` which on input `x` and random `e`
  outputs accepting conversation `(a, e, z)` with same probability-distribution as conversations between
  honest `Pr`, `Vr` on input `x` (so honest `Vr` cannot distinguish)

### Schnorr's protocol

(Damgard 1)

A simple example Sigma protocol.

- `x` = `(p, q, g, h)`, `w <- Z_q` where `p` is prime, `q` a prime divisor in `p - 1`, `g` an order-q element in `Z_p*`, `h = g^w mod p`
- `Vr` with `(p, q, g, h)` can check `p, q` prime, `g, h` have order `q`
  Since only one subgroup of order `q` in `Z_p*`, exists `w` such that `h = g^w`

Goal of protocol: `Pr` prove their knowledge of `w` to `Vr`.

Three steps:
- `Pr` chooses `r` at random in `Z_q`, sends `a = g^r mod p` to `Vr`.
- `Vr` chooses random `e <- Z_2^t` (`t` is free parameter but must have `2^t < q` such that `e <- Z_q`)
- `Pr` sends `z = r + ew mod q` to `Vr`, `Vr` checks `g^z = ah^e mod p`.

*Completeness*

`g^z` = `g^r g^ew` = `a * (g^w ^ e)` = `a * h^e`.

*Special soundness*

If `Pr` answers `e, e'`, then we have:
1. `(z, z')` s.t. `g^z = ah^e mod p`, `g^z' = ah^e' mod p`
2. `g^(z - z') = h^(e - e') mod p` (divide equations)
3. `h = g^(z-z')(e-e')^(-1) mod p`, so `w = (z-z')(e-e')^-1 mod q`.

Error probability of `1/2^t` (inverse of the size of the challenge space).

*Honest-verifier ZK*

Choose random `z <- Z_p*`, `e <- Z_q`, compute `a = g^z h^-e mod p`.

Notes:
- *Not* malicious-verifier zero-knowledge in basic construction since `q`, `2^t` must be exponentially large
- `p, q` can be constant for many protocols with different `w`s

### Properties of Sigma protocols

(Damgard 2)

- Can be composed in parallel (`Vr` sends `a, a', e, e'`, `Pr` sends `z, z'`) (note: not `a` twice)
- Can be shortened (`Pr` just appends zeroes to `e`).

So we can choose the challenge length.

### OR-construction

Given `x_0, x_1`, `Pr` wants to prove knowledge of `w` such that either `(x_0, w) <- R` or `(x_1, w) <- R` without revealing which is known.

Assume:
- Sigma protocol `P` for `R`.
- `x_0, x_1` known to `Pr`, `Vr`
- `Pr` knows `w` with `(x_b, w) <- R` where `b <- {0, 1}`

Intuition: `Pr` completes two instances of `P` for `x_0` / `x_1`, one for real, one with simulator `M`.

Construction (`P_OR`):
1. Computation of `a`
   1. `Pr` computes with `x_b, w` `a_b` using `P`
   1. `Pr` chooses `e_1-b` at random, runs simulator `M` on `x, e_1-b` producing `(a_1-b, e_1-b, z_1-b)`
   1. `Pr` sends `a_0, a_1` to `Vr`
2. Challenge `e`
   1. `Vr` chooses `t`-bit string `s`, sends to `Pr`
3. Response
   1. `Pr` sets `e_b = s xor e_1-b`, computes `z_b` in `P` to challenge `e_b` with `x_b, a_b, e_b, w`.
   1. `Pr` sends `e_0, z_0, e_1, z_1` to `Vr`
   1. `Vr` checks that `s = e_0 xor e_1`, that `(a_0, e_0, z_0)` and `(a_1, e_1, z_1)` accept in `P` on `x_0`, `x_1`.

`P_OR` is a Sigma-protocol, and for any verifier `Vr`, probability distribution of conversations is independent of `b` ("witness indistinguishable").

Completeness by construction.

*Special soundness*

Let two accepting conversations be `(a_0, a_1, s, e_0, e_1, z_0, z_1)` and `(a_0, a_1, s', e'_0, e'_1, z'_0, z'_1)` w/ `s /= s'`.
For `c <- {0, 1}`, `e_c /= e_c'`, then from `(a_c, e_c, z_c)` and `(a_c, e'_c, z'_c)` we can compute `w` from `P`.

*Honest-verifier ZK*

Given `s`, choose `e_0, e_1` at random with `s = e_0 xor e_1`, run `M` twice on `(x_0, e_0)` and `(x_1, e_1)`.

*Witness indistinguishable*

Given `V*`, conversations are of form `a_0, a_1, s, e_0, e_1, z_0, z_1`, `a_0` and `a_1` are chosen by honest `Pr`. `s` has distribution determined by `V*`.
`e_0, e_1` are random. `z_0`, `z_1` have distribution determined by honest `Pr` - true for `z_b`, follows from honest-verifier zero-knowledge for `z_1-b`, so distribution doesn't depend on `b`.

### Hard relations

Intuition: want finding `w` given `x` to be hard (otherwise `Vr` could just find `w`)

`R` is "hard" iff:
- P.P.T algorithm `G` - "generator" - on input `1^k` outputs `(x, w) <- R` where `|x| = k`.
- For all p.p.t. `A`, if we run `G` on input `1^k`, give `A` the `x` resulting in `w_a`, with `p_A(k)`
  the chance that `(x, w_A) <- R`, then `p_A(k)` is negligible in `k`. (negligible = grows slower than reciprocal of polynomial)

Consider this game:
- Run `G` on `1^k` to get pairs `(x, w) <- R`.
- Let `Vr` execute `P` with `Pr` poly times on `x`
- `Vr` outputs `w*`

We care about: is `(x, w*) <- R`?

Sigma protocol `P` is witness-hiding iff any poly-time `Vr` wins this game with negligible probability.

If `R` is a hard relation, `P_OR` is witness-hiding:

Assume `V*` that can win the game.

1. Run `G`, get `(x, w) <- R`.
2. Choose `b` at random.
3. Let `V*` run, output `w*`.

If `V*` wins, probability `(x, w*) <- R` >= `1/2`. So we can get `w` with `e / 2`, and `R` is not hard.

### Deriving ZK

Use `G` and `P_OR`. To derive ZK from some Sigma protocol `P`:

Suppose public `x` and private `w` to prover `Pr.

1. `Vr` runs `G` on `1^k` to get `(x', w') <- R`.
2. `Vr` sends `x'` to `Pr`, proves using `P` knowledge of `w'`. (note `Vr` is proving!)
3. `Pr` checks acceptance with `P` then proves `w | w'` with `P_OR`.

*Malicious-verifier zero-knowledge*

Simulator against `Vr` - rewind `Vr` to extract `w'`, run step 3 using knowledge of `w'`.
By witness indistinguishability, prover knowing `w` or `w'` are not distinguishable.

(no formal proofs in this paper)

### Commitment schemes

Hard relation `R`, generator `G`, Sigma protocol `P`.

*Setup*

`Vr` runs `G` to get `(x, w) <- R`, sends `x` to `Pr`.

*Commit*

`Pr` runs `M` on `x`, `e`, gets `(a, e, z)`, sends `a` to `Vr`.

*Open*

`Pr` sends `e, z` to `Vr`, `Vr` checks that `P` accepts `(a, e, z)`

Binding: if could be opened to `e`, `e'` with `e /= e'` then we would have `(a, e, z)`, `(a, e', z')` and could compute `w`.

Why would you use a Sigma protocol instead of a hash function?
- Perfectly hiding.
- Efficient if `P` is efficient.

### Fiat-Shamir transform

Random oracle `R : {0, 1}^l -> {0, 1}^t` for some `l`, `t`

`R` accessible to both `Pr`, `Vr`, `R` returns same output for same input, but output is totally random conditional on input.

Instead of `Vr` choosing random challenge `e`, use `R` to compute `e` from `a`.

Then `Pr` can produce `(a, z)` and `Vr` can compute `e = R(a)` and check `z` asynchronously (e.g. `Vr` is a blockchain client).

`Pr` could generate many `a`s and try to find an `e` he can answer - but if the number of possible `e`s is exponential, requires exponential calls to `R`.

This *also* forces `Vr` to be honest - since `e` is random conditioned on `a` - so Sigma protocols are honest in random-oracle.

Random oracles in practice: hash functions (but imperfect).
