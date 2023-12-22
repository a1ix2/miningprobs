# miningprobs
Mining probabilities and statistics with a focus on xmr p2pool

## Effort distribution

Mining blocks/share can be approximated by a Poisson point process. The uniformity property of the hash function tells us that for a fixed difficulty `diff` the probability of finding a block in a single attempt `p = 1/diff`. Therefore the probability `P(n)` of finding a block after `n` failed attempt is given by the geometric distribution
```
P(n) = (1 - p)(1 - p)...(1 - p) p = (1-p)^n p.
       \________n times_______/
```
Let `p = 1/diff = (n/diff)/n`. By taking the limit `n -> inf` of a large number of attempts while keeping the ratio `n/diff` finite we find
```
P(n)dn = (1/diff) exp(-n/diff) dn
```
where the infinitesimal `1/n -> dn`. The ratio `n/diff` is nothing but the effort (technically `(n+1)/diff` but we don't care for large `n`), i.e. the number of attempted hashes before the next block/share is found in units of the difficulty. Let `f = n/diff`. With this change of variable we get the effort distribution
```
P(f)df = exp(-f)df,
```
i.e. the standard exponential. [Figure 1](https://imgur.com/a/BCfwP41) compares the empirical distribution of the efforts of the last 100 blocks on p2pool and p2pool mini (top row), and the last 49 shares of two randomly selected miners on p2pool and p2pool mini respectively (bottom row). All 4 empirical distributions pass the Kolmogorov-Smirnov test, i.e. fail to reject the null hypothesis that the sample of efforts could have come out of a standard exponential. 

It follows that the CDF
```
P(f <= F) = integral exp(-f) over f from 0 to F
          = 1 - exp(-F).
```
and thus the quantiles
```
Q(x) = -log(1 - x).
```
Alternatively, we could focus on the change of variable `n = ht` involving the product of the hash rate `h` and `t` the inter-block time. With this change of variables we find the distribution of inter-block times
```
P(t)dt = (h/diff) exp(-(h/diff)t) dt,
```
namely the exponential distribution with rate `h/diff`.

Compared to the inter-block times distribution, the effort distribution has two favourable properties. It does not depend on the difficulty of the network and is therefore static over time, and it also does not depend on the local hash rate and is thus universal across pools and miners.

Some effort quantiles of interest are
```
Q(0.05) = 5%
Q(0.25) = 29%
Q(0.50) = 69%
Q(0.63) = 100%
Q(0.75) = 139%
Q(0.86) = 200%
Q(0.95) = 300%
```
These tell us that the odds below:above of an effort of 100% are roughly 2:1, and the odds below:above 200% are roughly 6:1. In other words mining blocks with efforts of 200% or more is not that atypical.

### Distribution of the number of shares/blocks for a given cumulative effort
 
Given once again the probability `p = 1/diff` of hitting a share, the probability of a single typical run `i` of `n` trials out of which `k` shares were found could look something like this
 
```
Q_i(k | n) = (1 - p)(1 - p) p (1 - p) p (1 - p)(1 - p) p p (1 - p)...(1 - p)(1 - p) = p^k (1-p)^(n-k)
           \____k successes/shares with prob p, n-k failures with prob 1 - p____/
```
 
To get the total probability of finding `k` shares within `n` attempts, we must account for all possible permutations `i` of failures and successes, i.e. all possible arrangement of `n-k` failure probability `(1 - p)`'s and `k` success probabilities `p`'s. There are n_choose_k of them, namely as many as there are ways of picking `k` positions among `n` possible positions. The probability of hitting `k` shares within `n` attempts is therefore given by the binomial distribution with success probability `p`:
```
                                   n!
P_exact(k shares | n hashes) = ---------- p^k (1 - p)^(n - k).
                               k!(n - k)!
```
Since the cumulative effort `f = np`, we can write `p^k = f^k/n^k`. For `n` much larger than `k` we have `n!/k!/(n-k)!/n^k -1`, and together with large difficulty, that is `p << 1`, we therefore find `(1-p)^(n-k) -exp(-f)`. Putting those limits together we find to a good approximation
```
                         f^k
P(k shares | effort f) = ---- exp(-f)
                          k!
```
which is nothing but the Poisson distribution. The mode of the Poisson distribution is `floor(f)`, the mean is `f`, and the standard deviation `sqrt(f)`. Using the CDF of the Poisson distribution, we find the probability of the d-th deviate
```
                                         Gamma(floor(f + d*sqrt(f) + 1), f)     Gamma(floor(f - d*sqrt(f)), f)
P(f - d*sqrt(f) <= k <= f + d*sqrt(f)) = ----------------------------------  -  ----------------------------------
                                           Gamma(floor(f + d*sqrt(f) + 1))        Gamma(floor(f - d*sqrt(f)))
```
where `Gamma(x, s)` is the upper incomplete gamma function. To give a practical example, this means that for a cumulative effort `f = 1500%` one will likely have found roughly 15 ± 4 blocks/shares with 63% probability (1st deviate), and 15 ± 8 blocks/shares with 95% probability (2nd deviate). As the cumulative effort `f` becomes large, those deviate probabilities will converge to those of the normal deviates, namely 68.3% at 1 standard deviation and 95.4% at 2 standard deviations. For d-th deviates with `d >= sqrt(2 + 1/f + f)` the lower bound in the above expression falls outside the support of the Poisson distribution, i.e. k < 0, and should instead be interpreted as 0. Computationally we simply replace every instances of `f - d*sqrt(f)` with `max(0, f - d*sqrt(f))` to keep the above expression valid for all `d 0`.
 
We can of course rewrite all the expressions above in terms of the local hash rate `h` and mining time `t` by substituting `n = ht` such that `f = ht/diff`. In particular
```
                     (ht/diff)^k 
P(k shares | h, t) = ----------- exp(-ht/diff).
                          k!
```

## Reference tables of #shares probabilities as a function of the cumulative effort. The last column represents the 1st deviate probabilities, i.e. the probability of falling within 1 standard deviation of the mean number of shares as denoted by square brackets.
```
nbshares       0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15 
  effort                                                                                                   P([bulk])
     25%    [78%]  19%    2%                                                                                   78%
     50%    [61%   30%]   8%    1%                                                                             91%
     75%    [47%   35%]  13%    3%    1%                                                                       83%
    100%    [37%   37%   18%]   6%    2%                                                                       92%
    150%    [22%   33%   25%]  13%    5%    1%                                                                 81%
    200%    [14%   27%   27%   18%]   9%    4%    1%                                                           86%
    300%      5%  [15%   22%   22%   17%]  10%    5%    2%    1%                                               77%
    400%      2%    7%  [15%   20%   20%   16%   10%]   6%    3%    1%    1%                                   80%
    500%      1%    3%   [8%   14%   18%   18%   15%   10%]   7%    4%    2%    1%                             83%
    600%            1%    4%   [9%   13%   16%   16%   14%   10%]   7%    4%    2%    1%    1%                 79%
    700%            1%    2%    5%   [9%   13%   15%   15%   13%   10%]   7%    5%    3%    1%    1%           75%
    800%                  1%    3%    6%   [9%   12%   14%   14%   12%   10%]   7%    5%    3%    2%    1%     72%
    900%                        1%    3%    6%   [9%   12%   13%   13%   12%   10%    7%]   5%    3%    2%     76%
   1000%                        1%    2%    4%   [6%    9%   11%   13%   13%   11%    9%    7%]   5%    3%     80%
nbshares       0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15 
```
## Approximate distribution of daily earnings in a PPLNS pool

We have just shown how the number of shares mined within a given timeframe behaves as a Poisson random variable with rate parameter `f`, the miner's cumulative effort, that depends on the miner's local hash rate `h`, the timeframe `t`, and the share difficulty `d` (short for `diff` from here on). As we've mentioned several times the principle is exactly the same for the distribution of the number of blocks mined by a pool in a given timeframe. One only has to use instead the pool's hash rate `H`, some other timeframe `T` of interest, and the block difficulty `D`.

Let `S` be the Poisson random variable representing the number of shares mined by a single miner within a PPLNS window of `N` last shares (e.g. 354 for p2pool and 2160 for p2pool mini). Let `B` be the Poisson random variable representing the number of blocks mined by a pool within a day. Without loss of generality set the block reward to 1. A reasonable approximation for the daily earnings of this miner is given by `BS/N`. This is an approximation because `S` is autocorrelated over time rather than IID, and that the `t` we use in the miner's effort parameter `f=ht/d` is actually a random variable itself that depends on `N` and the pool's total hash rate `H`. We are justified in making this approximation if we can assume that the pool adjusts the difficulty in a way that `N` shares are on average mined within a fixed time frame and that fluctuations in `t` are minimal. More generally, there is the consideration of the pool difficulty `D` and pool hash rate `H`. Those values vary as miners come and go and as the global block difficulty `D` varies over time. We will assume that there ratio remains approximately constant as solo miners are incentivised to join or leave pools when the block difficulty increases to much or drops down in relation to their hash rate. With all those assumptions in mind, the probability distribution of daily earnings `r`
```
P( r | f=ht/d, F=HT/D ) = sum_b=0^inf sum_s=0^inf dirac(r - bs/N) Ppois( s | f ) Ppois( b | F)

                                                       f^s   F^b
                        = sum_b sum_s dirac(r - bs/N) ----- ----- exp(-f)exp(-F).
                                                        s!    b!
```
Instead of working with the probability distribution directly, let's write the above in terms of the its characteristic function with auxiliary variable `z` for daily earnings. One can always invert the characteristic function to recover the probability distribution; they are dual to each others. The characteristic function
```
                 inf  inf              f^s   F^b
E[exp(izBS/N)] = sum  sum exp(izbs/N) ----- ----- exp(-f)exp(-F).
                 b=0  s=0               s!    b!
```
Up to now this expression is "exact" for the approximate daily earnings. Let's find a quick and dirty continuous approximation by moment matching using an unjustified, physicist-coded approach that would make R. Fisher cry. Using the Taylor expansion of `exp(izbs/N)`
```
                                                                        f^s   F^b
E[exp(izBS/N)] = sum_b sum_s (1 + izbs/N + (iz)^2 b^2 s^2 / N^2 + ...) ----- ----- exp(-f)exp(-F).
                                                                         s!    b!
```
The n-th non-central moments of the Poisson distribution with rate `lambda` are given by the Touchard polynomials
```
                n
T(n, lambda) = sum S(n, k) lambda^k,
               k=0
```
where `S(n, k)` are the Stirling numbers of the second kind. In the following we will only need the first two Touchard polynomials, which for reference are
```
T(1, lambda) = lambda,
T(2, lambda) = lambda * (1 + lambda).
```
We can therefore write
```
                 inf  1 (iz)^k 
E[exp(izBS/N)] = sum -- ------ T(k, f) T(k, F)
                 k=0  k!  N^k
```
When looking at the original expansion of the distribution of earnings, we see from the sum over `b` and `s` of `bs/N` that the distribution will consist of one point mass at 0 whenever `b=0`, `s=0`, or both `b=s=0`, followed by a dust of point masses at non-zero rational numbers. Intuitively, as `b` and `s` increase, the relative distances between neighboring point masses within that dust tend to zero. The central limit theorem would therefore suggest a continuous limit exists and should converge to a continuous distribution with support over the positive real numbers, usually something like a log-normal or a gamma distribution. Those distribution do not give any weight to 0. We will therefore first isolate the 0 component as follows

```
                                /      1     inf  1 (iz)^k                \
E[exp(izBS/N)] = p0 + (1 - p0) | 1 + ------  sum -- ------ T(k, f) T(k, F) |
                                \    1 - p0  k=1  k!  N^k                 /
```
where we find from the original expression for `P(r)` thatx
```
p0 = exp(-f - F) + exp(-f)(1 - exp(-F)) + exp(-F)(1 - exp(-f)),
   = exp(-f - F)(exp(F) + exp(f) - 1).
```

We now perform some street magic: pretend that this characteristic function belongs to a continuous distribution, notice that the constant `p0` gives us the point mass at 0, and claim that the non-zero component can be well approximated by a gamma distribution for some shape `k` and scale `A`. Symbolically
```
E[exp(izBS/N)] ~ p0 + (1 - p0) (1 - izA)^-k.
```
Inverting this characteristic function gives us the approximate probability distribution of earnings `r`
```
P( r | f=ht/d, F=HT/D ) ~ p0 dirac(r) + (1 - p0) Gamma(r | k, A).
```
The gamma distribution has
```
mode = (k - 1)A,   k >= 1,
     = kA,         0 < k < 1,

mean = kA,

stdev = sqrt(k)A.
```
With this subterfuge in place, expand both the exact characteristic function and the characteristic function of the zero-inflated gamma distribution and match the first two non-central moments (the constant terms are trivially equal):
```math
kA = \frac{1}{1 - p_0} \frac{fF}{N}
```
```math
k(1+k)A^2 = \frac{1}{1-p_0} \frac{f F(1 + f)(1 + F)}{N^2}
```

<!-- ```
                1    f F
         kA = ------ ---,
              1 - p0  N 

                1    fF(1 + f)(1 + F)
k(1 + k)A^2 = ------ ----------------.
              1 - p0       N^2
``` -->
Solve for `k` and `A` and find
```
                               f F                           
k = ---------------------------------------------------------,
    (F + 1)(f + 1)(exp(-f-F) - exp(-F) - exp(-f)) + F + f + 1

    F + f + 1    F f (exp(F) + exp(f) + 1)
A = --------- - ---------------------------.
        N        N (exp(F) - 1)(exp(f) - 1)
```
Direct your public's attention to the fact that both `k` and `A` share the same symmetry under the interchange `h <-> H` as the original exact expression. This is desirable. For large-ish values of `f`, namely for miners who find at least some shares within the PPLNS window, `k` and `A` reduce to the much simpler expressions `k = f/N` and `A = F`. This limit breaks the symmetry, but as we will see, does not substantially degrade the quality of the approximation in realistic empirical scenarios.

## Synthetic evaluation of the zero-inflated gamma approximation

In the current state of the Monero network, the network difficulty over the last year (2023) hovers around `D ~ 300G`. Using p2pool mini as an example, the PPLNS window is 6 hours which sets the share difficulty at roughly `d ~ 90M` given its current hash rate of around `H ~ 9MH/s` and window length `N = 2160`. The daily effort of p2pool mini is therefore around
```
    24 * 3600 * 9e6
F = --------------- = 2.6
         300e9
```
and the window effort of a miner with local hash rate of `50 kH/s`
```
    6 * 3600 * 50e3
f = --------------- = 12.0
         90e6
```
Remember from the previous sections that this means the pool would find on average 2.6 blocks per day, and the miner finds on average 12.6 shares within the PPLNS window.