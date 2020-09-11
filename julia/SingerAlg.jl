#############################################################################
##
##  SingerAlg.jl
##
##  Examine the Loewy structure of the algebras ``A[q,z] = A(q,n,e)``.
##

"""
  SingerAlg.jl provides Julia code for computations concerning the algebras
  ``A[q,z]`` that are studied in the following papers.

  [BHHK20a](https://doi.org/10.1016/j.jalgebra.2019.05.004)
  Breuer, T., Héthelyi, L., Horváth, E. and Külshammer, B.,
  The Loewy Structure of Certain Fixpoint Algebras, Part I,
  J. Algebra 558 (2020), 199–220.

  [BHHK20b](http://arxiv.org/abs/1912.03065v1)
  Breuer, T., Héthelyi, L., Horváth, E. and Külshammer, B.,
  The Loewy Structure of Certain Fixpoint Algebras, Part II,
  http://arxiv.org/abs/1912.03065v1.

  The Julia functions are used by the
  [GAP](https://www.gap-system.org) package
  [SingerAlg](http://www.math.rwth-aachen.de/~Thomas.Breuer/singeralg).
"""
module SingerAlg

##  One reason for importing `GAP.jl` is that we want to set some
##  GAP attributes from the Julia code as soon as the values become known.
##  Another reason is that we want to use GAP's `Factors` function
##  for decomposing integers into primes.
import GAP

## The factorization functions in Julia's `Primes` package (at least up to
## version 0.4.0) are substantially slower than the ones in GAP.
## We use the relevant GAP functions via GAP.jl.
include( "PrimesGAP.jl" )

include( "utils.jl" )
include( "loewy.jl" )
include( "mindeg.jl" )
include( "invar.jl" )

end

