
"""
  `SingerAlg.PrimesGAP.jl` provides a few functions based on GAP
  that are faster than the corresponding functions from the Julia package
  `Primes.jl`.
  Since one of the aims of GAP's `SingerAlg` package is
  to compare implementations in GAP with Julia implementations,
  we use GAP's factorization code in both situations.
"""
module PrimesGAP

import GAP

isprime(n::T) where {T<:Integer} = GAP.Globals.IsPrimeInt(GAP.julia_to_gap(n))

nextprime(n::T) where {T<:Integer} = GAP.Globals.NextPrimeInt(GAP.julia_to_gap(n-1))

factors(n::T) where {T<:Integer} = Vector{Int}(GAP.Globals.Factors(GAP.julia_to_gap(n)))
#T We would better provide a function `factor` as in `Primes.jl`,
#T to whose result one can apply `sort(collect(Set, .))`.

end

