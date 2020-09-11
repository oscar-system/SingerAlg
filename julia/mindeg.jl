
"""
    MinimalDegreeOfSingerAlgebraCache

a dictionary that stores at the key `e` (an integer), if bound,
a dictionary that stores at the key `q` (an integer), if bound,
the value `m(q,e)`;
the values get stored by calls of `MinimalDegree`.
"""
MinimalDegreeOfSingerAlgebraCache = Dict{Integer,Dict{Integer,Integer}}();


"""
    VectorIterator( m, n, s )

Iterate over all vectors of length `n`
with entries in the set { 0, 1, 2, ... `m` }
and coefficient sum `s`.
The vectors are enumerated in lexicographical order.
(The functions change the vectors in place,
just collecting the results makes no sense.)
"""
mutable struct VectorIterator
    m::Int
    n::Int
    s::Int
end

# Auxiliary function for the iterator:
# Distribute `s` to the first `n` positions in the array `v`
# such that as many initial entries as possible are `m`.
function VectorIterator_ResetPrefix(v::Vector{Int}, m::Int, n::Int, s::Int)
    local rest::Int, i::Int, j::Int

    rest = s
    i = 1
    while m < rest
      v[i] = m
      rest = rest - m
      i = i + 1
    end
    v[i] = rest
    for j in (i+1):n
      v[j] = 0
    end

    return v
end

# Initialize the iterator.
function Base.iterate(vi::VectorIterator)
    local next::Vector{Int}

    if vi.s > vi.m * vi.n
      # The iterator is empty.
      return
    end
    next = VectorIterator_ResetPrefix(zeros(Int, vi.n), vi.m, vi.n, vi.s)
    return (next, next)
end

# Define the iteration step.
# Note that `state` is changed in place.
function Base.iterate(vi::VectorIterator, state::Vector{Int})
    local sum::Int, i::Int

    # Find the first position with a nonzero value
    # such that the value on the right is smaller than m.
    sum = -1
    for i in 1:(vi.n-1)
      sum = sum + state[i]
      if state[i] != 0 && state[ i+1 ] < vi.m
        state[i] = state[i] - 1
        state[i+1] = state[i+1] + 1
        VectorIterator_ResetPrefix(state, vi.m, i, sum)
        return (state, state)
      end
    end

    # There is no such position, we are done.
    return
end

"""
    MinimalDegreeCheap(q::Int, n::Int, e::T) where {T<:Integer}

Return either 0 or the number ``m(q,e)``,
which is the smallest number of powers of `q`
such that `e` divides the sum of these powers.

The return value 0 means that the cheap criteria from the two papers
do not suffice to determine ``m(q,e)``.

The argument `n` must be `OrderMod(q, e)`.
"""
function MinimalDegreeCheap(q::Int, n::Int, e::T) where {T<:Integer}
    if e == 1
      return 1
    end

    if q == 1
      # Example I.6.1.
      return Int(e)
    elseif iseven(n) && powermod(q, div(n, 2), e) == e-1
      # Decide the case m = 2, see Lemma I.6.3:
      return 2
    elseif n == 2
      # Prop. II.2.7.
      e1 = Int(gcd(e, q-1))
      e2 = Int(gcd(e, q+1))
      if e2 <= e1 || (iseven(e) && iseven(div(q^2-1, e)))
        return e1
      else
        return 2 * e1
      end
    end

    # Deal with those cases where e is a prime power,
    # and where we know the value of m(q,e).
    if IsPrimePowerInt(e)
      # Let e = p^k for a prime p.
      if mod(e, 2) == 0
        # see Prop. II.2.9, note that here we have e > 4 and q mod e <> q-1.
        if mod(q, 4) == 1
          return Int(gcd(e, q-1))
        else
          @assert (mod(q+1, e) != 0) "the case m = 2 cannot occur here"
          return 4
        end
      else
        # see Prop. II.2.10, note that q \equiv 1 \pmod{p}
        # if and only if \gcd( e, q-1 ) \not= 1 holds.
        m = Int(gcd(e, q-1))
        if m != 1
          return m
        elseif mod(n, 3) == 0
          # Here we know that m(q,e) > 2 and thus p \not= 3.
          # Hence \gcd( e, q^{n/3}-1 ) = 1 holds,
          # because otherwise q^{n/3} would be a p-element in (Z/eZ)^*.
          # Thus Lemma I.6.4 yields m(q,e) \leq 3.
          # (This is a generalization of Cor. II.2.11.)
          return 3
        elseif 2 * n == Phi(e)
          # Cor. II.2.14.
          return 3
        elseif mod(e, 11) == 0
          # Prop. II.2.16.
          return 5
        end
      end
    end

    # Deal with those cases where e is twice an odd prime power,
    # and where we know the value of m(q,e).
    if iseven(e) && IsPrimePowerInt(div(e, 2))
      # see Prop. II.2.17, note that the order of q mod e is a power of p
      # if and only if \gcd( e/2, q-1 ) \not= 1 holds.
      m = gcd(e, q-1)
      if m > 2
        return Int(m)
      end
    end

    # We give up.
    return 0
end;


"""
    MinimalDegreeHard(q::Int, n::Int, e::T) where {T<:Integer}

Return the number `m(q,e)`, which is the smallest number of powers of `q`
such that `e` divides the sum of these powers.
The argument `n` must be `OrderMod( q, e )`.
"""
function MinimalDegreeHard(q::Int, n::Int, e::T) where {T<:Integer}
    # If A[q,n,z] has small dimension and e is not very small
    # then enumerate its basis.
    # (For small e, we known that the m values are small,
    # so it is cheaper not to enumerate.)
    if e > 100
      z = div(BigInt(q)^n - 1, e)
      if z <= 10^5
        return LoewyStructureInfo(q, n, Int(z))[:m]
      end
    end

    # m(q,e) is divisible by \gcd( e, q-1 ), and m(q,e) \geq 3.
    e1 = Int(gcd(e, q-1))
    if e1 == 1
      m = 3
    elseif e1 == 2
      m = 4
    else
      m = e1
    end

    powers = [powermod(q, i, e) for i in 1:(n-1)]

    while true
      for a in 1:m
        for v in VectorIterator(a, n-1, m-a)
          if mod(a + sum(v .* powers), e) == 0
            return m
          end
        end
      end
      m = m + e1
    end
end;


"""
    MinimalDegree(q::Int, e::T) where {T<:Integer}
    MinimalDegree(q::Int, n::Int, e::T) where {T<:Integer}
    MinimalDegree(q::Int, n::Int, e::T, A::GAP.GapObj) where {T<:Integer}

Return the number `m(q,e)`, which is the smallest number of powers of `q`
such that `e` divides the sum of these powers.

If `n` is given, it must be the multiplicative order of `q` modulo `e`.
"""
function MinimalDegree(q::Int, e::T) where {T<:Integer}
    return MinimalDegree(q, OrderMod(q, e), e)
end;

function MinimalDegree(q::Int, n::Int, e::T) where {T<:Integer}
    q = Int(mod(q, e))

    if ! haskey(MinimalDegreeOfSingerAlgebraCache, e)
      cache_e = Dict{Integer,Integer}()
      MinimalDegreeOfSingerAlgebraCache[e] = cache_e
    else
      cache_e = MinimalDegreeOfSingerAlgebraCache[e]
      if haskey(cache_e, q)
        return cache_e[q];
      end
    end

    m = MinimalDegreeCheap(q, n, e)
    if m == 0
      m = MinimalDegreeHard(q, n, e)
    end

    # Extend the cache.
    cache_e[q] = m

    return m
end;

function MinimalDegree(q::Int, n::Int, e::T, A::GAP.GapObj) where {T<:Integer}
    # Compute the value.
    m = MinimalDegree(q, n, e)

    # Set the attribute value in A.
    GAP.Globals.SetMinimalDegreeOfSingerAlgebraJulia(A, m)

    return m
end;

