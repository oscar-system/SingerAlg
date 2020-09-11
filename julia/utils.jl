"""
    PrimeDivisors(n::T) where {T<:Integer}

Return the sorted array of prime divisors of `n`.
"""
function PrimeDivisors(n::T) where {T<:Integer}
    GAP.gap_to_julia(Vector{T}, GAP.Globals.PrimeDivisors(GAP.julia_to_gap(n)))
end;


function RootInt(n, k = 2)
    # check the arguments and handle trivial cases
    if k <= 0
      error("<k> must be positive")
    elseif k == 1
      return n
    elseif n < 0 && mod(k, 2) == 0
      error("<n> must be positive")
    elseif n < 0 && mod(k, 2) == 1
      return -RootInt(-n, k)
    elseif n == 0
      return 0
    elseif n <= k
      return 1
    end

    # r is the first approximation, s the second, we need: root <= s < r
    r = n
    s = BigInt(2)^(div(LogInt(n, 2), k)+1)-1

    # do Newton iterations until the approximations stop decreasing
    while s < r
      r = s
      t = r^(k-1)
      s = div(n+(k-1)*r*t, k*t)
    end

    # and that's the integer part of the root
    return r
end


"""
    SmallestRootInt(n::T) where {T<:Integer}

The smallest root of an integer `n` is the integer `r` of smallest absolute
value for which a positive integer `k` exists such that `n == r^k`.
"""
function SmallestRootInt(n::T) where {T<:Integer}
    # check the argument
    if n > 0
      k = 2
      s = 1
    elseif n < 0
      k = 3
      s = -1
      n = -n
    else
      return 0
    end

    # exclude small divisors, and thereby large exponents
    if mod(n, 2) == 0
      p = 2
    else
      p = 3
      while p < 100 && mod(n, p) != 0
        p = p+2
      end
    end
    l = LogInt(n, p)

    # loop over the possible prime divisors of exponents
    # use Euler's criterion to cast out impossible ones
    while k <= l
      q = 2*k+1
      while ! PrimesGAP.isprime(q)
        q = q+2*k
      end
      if powermod(n, div(q-1, k), q) <= 1
        r = RootInt(n, k)
        if r ^ k == n
          n = r;
          l = div(l, k)
        else
          k = PrimesGAP.nextprime(k+1)
        end
      else
        k = PrimesGAP.nextprime(k+1)
      end
    end

    return s * n
end


"""
    IsPrimePowerInt(n::T) where {T<:Integer}

Return `true` if `n` is a prime power, and `false` otherwise.
"""
IsPrimePowerInt(n::T) where {T<:Integer} = PrimesGAP.isprime(SmallestRootInt(n))


"""
    Lambda(m::T) where {T<:Integer}

Return the exponent of the group of prime residues modulo the integer `m`.
"""
function Lambda(m::T) where {T<:Integer}
    # make <m> it nonnegative, handle trivial cases
    if m < 0
      m = -m
    end
#   if m < Length(PrimeResiduesCache) then
#     return Length(PrimeResiduesCache[m+1]);
#   end
    # loop over all prime factors $p$ of $m$
    lambda = 1
    for p in PrimeDivisors(m)
        # compute $p^e$ and $k = (p-1) p^(e-1)$
        q = p
        k = p-1
        while mod(m, (q*p)) == 0
          q = q * p
          k = k * p
        end
        # multiples of 8 are special
        if mod(q, 8) == 0
          k = div(k, 2)
        end
        # combine with the value known so far
        lambda = lcm(lambda, k)
    end
    return lambda
end


"""
    LogInt(n::Int, base::Int)

Return the integer part of the logarithm of the positive integer `n`
with respect to the positive integer `base`,
i. e., the largest positive integer ``e`` such that ``base^e \\leq n``.
"""
LogInt(n::T, base::Int) where {T<:Integer} = Int(floor(log(base, n)))


"""
    OrderMod(n::Int, m::T1, bound::T2 = Lambda(m)) where {T1<:Integer} where {T2<=Integer}

Return the multiplicative order of the integer `n` modulo the
positive integer `m`, i. e.,
the smallest positive integer ``i`` such that ``n^i \\equiv 1 \\pmod{m}``.
The function returns `0` if `n` and `m` are not coprime.
"""
function OrderMod(n::Int, m::T1, bound::T2 = Lambda(m)) where {T1<:Integer} where {T2<:Integer}
    # check the arguments and reduce $n$ into the range $0..m-1$
    if m <= 0
      error("<m> must be positive")
    end
    if n < 0
      n = mod(n, m) + m
    end
    if m <= n
      n = mod(n, m)
    end

    # return 0 if $m$ is not coprime to $n$
    if gcd(m, n) != 1
      o = 0

    # compute the order simply by iterated multiplying, $x= n^o$ mod $m$
    elseif m < 100
      x = n
      o = 1
      while x > 1
        x = mod( x * n, m )
        o = o + 1
      end

    # otherwise try the divisors of $\lambda(m)$ and their divisors, etc.
    else
      o = bound
      for d in PrimeDivisors(o)
        while mod(o, d) == 0 && powermod(n, div(o, d), m) == 1
          o = div(o, d)
        end
      end
    end

    return Int(o)
end;


"""
    DuplicateFree(v)

Return a vector that contains the same entries as the array `v`,
but each entry occurs exactly once.
"""
function DuplicateFree(v)
  v1 = Vector{eltype(v)}()
  if length(v) > 0
    push!(v1, v[1])
    for elm in v
      if ! (elm in v1)
        push!(v1, elm)
      end
    end
  end
  return v1
end


# recursive function used by `DivisorsInt`
function DivisorsIntInner(i::Int, m::Int, factors::Vector{Int})
    if length(factors) < i
      return [m]
    elseif mod(m, factors[i]) == 0
      return DivisorsIntInner(i+1, m*factors[i], factors)
    else
      res = DivisorsIntInner(i+1, m, factors)
      append!(res, DivisorsIntInner(i+1, m*factors[i], factors))
      return res
    end
end

"""
    DivisorsInt(n::Int)

Return an array of all divisors of the integer `n`.
This array is sorted, so that it starts with `1` and ends with `n`.
We define that `DivisorsInt( -n ) == DivisorsInt( n )`.
"""
function DivisorsInt(n::Int)
    # make <n> nonnegative, handle trivial cases, and get prime factors
    if n < 0
      n = -n
    end
    if n == 0
      error("DivisorsInt: <n> must not be 0")
    end
#   if n <= length(DivisorsIntCache)
#     return DivisorsIntCache[n]
#   end
    factors = PrimesGAP.factors(n)

    return sort(DivisorsIntInner(1, 1, factors))
end


"""
    Phi(m::T) where {T<:Integer}

Return the value of Euler's totient function of `m`.
"""
function Phi(m::T) where {T<:Integer}
    # make <m> nonnegative, handle trivial cases
    if m < 0
      m = -m
    end

#   if m < Length(PrimeResiduesCache) then 
#     return Length(PrimeResiduesCache[m+1]);  
#   fi;

    # compute $phi$
    phi = m
    for p in PrimeDivisors(m)
      phi = div(phi, p) * (p-1)
    end

    # return the result
    return phi
end

