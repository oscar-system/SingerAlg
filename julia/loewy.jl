"""
    CoefficientsQadicReversed(k::Int, z::T, q::Int, n::Int) where {T<:Integer}

Return an array ``[ v_1, v_2, \\ldots, v_n ]`` of length `n`
such that ``k*(q^n-1)/z`` has the form
``v_n q^0 + v_{n-1} q^1 + \\cdots + v_1 q^{n-1}``,
that is, the result contains the `q`-adic coefficients of ``k*(q^n-1)/z``
in reversed order.

We assume that ``0 \\leq k \\leq z``, ``q > 1``, and ``n > 0``.

The (perhaps large) number ``k*(q^n-1)/z`` need not be created
in the computations.
"""
function CoefficientsQadicReversed(k::Int, z::T, q::Int, n::Int) where {T<:Integer}
    v::Vector{Int} = zeros(Int, n)
    if k == z
      for i in 1:n
        v[i] = q-1
      end
    else
      r = k
      for i in 1:n
        d, r = divrem(r * q, z)
        v[i] = d
      end
    end

    return v
end


# The following function is needed only for the sake of efficiency
# comparisons.
function CoefficientsQadic(ke::T, q::Int, n::Int) where {T<:Integer}
    v::Vector{Int} = zeros(Int, n)
    for i in 1:n
        ke, r = divrem(ke, q)
        v[i] = r
    end

    return v
end


function islessorequal(mon1::Vector{Int}, mon2::Vector{Int}, n::Int)
  for i in 1:n
    if mon2[i] < mon1[i]
      return false
    end
  end

  return true
end

"""
    LoewyStructureInfo(q::Int, n::Int, z::T) where {T<:Integer}

Return a dictionary with the following keys.

`:monomials`
    the array of reversed `q`-adic expansions for multiples of
    `e = (q^n-1)/z`, each of length `n`,
    see [`CoefficientsQadicReversed`](@ref),

`:layers`
    the array of the Loewy layers to which the monomials belong,

`:chain`
    an array of positions of monomials of a longest ascending chain,

`:m`
    the value `m(q,e)`,

`:LL`
    the Loewy length of `A[q,n,z]`,
    equal to the length of the `:layers` value plus 1,

`:parameters`
    the array `[ q, n, z ]`.
"""
function LoewyStructureInfo(q::Int, n::Int, z::T) where {T<:Integer}
    if (z != 1 && powermod(q, n, z) != 1)
      error("<z> must divide <q>^<n> - 1")
    end

    monomials = [zeros(Int, n)]
    layers = Int[0]
    degrees = Int[0]    # just for speedup: avoid comparisons
    predecessors = Int[0]

    m = n * (q-1)
    for i in 1:z
      mon = CoefficientsQadicReversed(i, z, q, n)
      lambda = 0
      pred = 1
      mm = Base.sum(mon)
      for j in 2:i
        if lambda < layers[j] && degrees[j] < mm &&
                                 islessorequal(monomials[j], mon, n)
          lambda = layers[j]
          pred = j
        end
      end
      push!(monomials, mon)
      push!(layers, lambda + 1)
      push!(degrees, mm)
      push!(predecessors, pred)

      if lambda == 0
        if mm < m
          m = mm
        end
      end
    end

    # Extract information about one longest chain.
    i = length(monomials)
    pred = Int[]
    while i > 0
      push!(pred, i)
      i = predecessors[i]
    end

    return Dict(:monomials => monomials,
                :layers => layers,
                :chain => pred,
                :m => m,
                :LL => layers[z+1] + 1,
                :parameters => [q, n, z])
end;


"""
    LoewyVector(data::Dict)

Return an array that stores at position `i+1` the dimension of the `i`-th
Loewy layer of the Singer algebra described by the dictionary `data`.
"""
function LoewyVector(data::Dict) Vector{Int}
    v = zeros(Int, data[:LL])
    for i in data[:layers]
      v[i+1] = v[i+1] + 1
    end

    return v
end;


function SingerAlgE(q::Int, n::Int, z::T) where {T <: Integer}
    return div(BigInt(q)^n-1, z)
end;


"""
    LoewyLength(q::Int, z::T) where {T <: Integer}
    LoewyLength(q::Int, n::Int, z::T, m::Int) where {T <: Integer}
    LoewyLength(q::Int, n::Int, z::T, A::GAP.GapObj) where {T <: Integer}
    LoewyLength(q::Int, n::Int, z::T, m::Int, A::GAP.GapObj) where {T <: Integer}

Return the Loewy length of the Singer Algebra ``A[q, z]``.

If `n` and `m` are given then `n` must be the multiplicative order of `q`
modulo `z`, and `m` must be the value ``m(q, e)``.

If `A` is given then it must be the GAP object that represents ``A[q, z]``;
in this case, stored attributes of `A` are used.
"""
function LoewyLength(q::Int, z::T) where {T <: Integer}
    n = OrderMod(q, z)
    return LoewyLength(q, n, z,
               MinimalDegree(q, SingerAlgE(q, n, z)))
end;

function LoewyLength(q::Int, n::Int, z::Int, m::Int)
    l = LoewyLengthCheap(q, n, z, m)
    if l == 0
      l = LoewyLengthHard(q, n, z, m)
    end

    return l
end;

function LoewyLength(q::Int, n::Int, z::T, A::GAP.GapObj) where {T <: Integer}
    # First we need m; we store this attribute in `A`.
    if GAP.Globals.HasMinimalDegreeOfSingerAlgebraJulia(A)
      m = GAP.Globals.MinimalDegreeOfSingerAlgebraJulia(A)
    else
      m = MinimalDegree(q, n, SingerAlgE(q, n, z), A)
    end

    if GAP.Globals.HasLoewyStructureInfoJulia(A)
      # The Loewy info is now known.
      l = GAP.Globals.LoewyStructureInfoJulia(A)[:LL]
    else
      l = LoewyLength(q, n, z, m, A)
    end

    return l
end;

function LoewyLength(q::Int, n::Int, z::T, m::Int, A::GAP.GapObj) where {T <: Integer}
    # Compute the value.
    l = LoewyLengthCheap(q, n, z, m)
    if l == 0
      info = LoewyStructureInfo(q, n, z)
      # Store the attribute.
      GAP.Globals.SetLoewyStructureInfoJulia(A, info)
      l = info[:LL]
    end

    return l
end;

"""
    SufficientCriterionForLoewyBoundAttained(q::Int, n::Int, z::T, m::Int) where {T <: Integer}

Return a string that describes the criterion from
[BHHK1](https://doi.org/10.1016/j.jalgebra.2019.05.004) or
[BHHK2](http://arxiv.org/abs/1912.03065v1)
from which it follows that the Loewy length of the algebra ``A[q,z]``
is equal to the upper bound ``\\lfloor n (q-1) / m \\rfloor + 1``,
where ``n = \\ord_z(q)``, ``e = (q^n-1) / z``, and ``m = m(q, e)``;
if no such criterion was found then the returned string is empty.
"""
function SufficientCriterionForLoewyBoundAttained(q::Int, n::Int, z::T, m::Int) where {T <: Integer}

    if n <= 3
      return "Cor. I.7.1 (n <= 3)"
    elseif z < 70
      return "z < 70"
    elseif m == 2
      return "La. I.6.3"
    elseif mod(q - 1, m) == 0
      return "Thm. I.7.1"
    elseif n * (q-1) < 3 * m
      return "La. I.7.1 (iii)"
    end

    qm = mod(q, m)
    if 1 < qm && div(n * (qm-1), m) == 1
      return "Prop. II.3.15"
    end

    bq = BigInt(q)
    qnm1 = bq^n - 1
    e = div(qnm1, z)

    if e <= 32
      return "Prop. II.6.1 (e <= 32)"
    elseif e <= 3 * m
      return "Prop. II.6.4"
    elseif n <= 5 && mod(div(qnm1, q-1), e) == 0
      return "Prop. II.5.3, II.5.6 (e | (q^n-1)/(q-1), n <= 5)"
    elseif mod(n, div(m * OrderMod(q, e, n), gcd(m, q-1))) == 0
      return "La. II.5.2 (ii)"
    end

    if mod(div(qnm1, q-1), e) == 0
      # we know m <= n
      if m >= n-1
        return "Prop. II.5.1 (ii)"
      elseif m == n-2 && mod(q, m) in 1:div(m+1, 2)
        return "Prop. II.5.1 (iii)"
      elseif mod(div((n-m) * (q-1), m), n-m) == 0
        return "Prop. II.5.1 (i)"
      end
    end

    if mod(n, m) == 0
      if mod(Base.sum([bq^div(i*n, m) for i in 0:(m-1)]), e) == 0
        return "La. II.4.1 for r = 1"
      end
      for r in DivisorsInt(n)
        if mod(div(n, r), m) == 0 &&
           mod(Base.sum([bq^div(n*i, m*r) for i in 0:(m-1)]), e) == 0
          return "La. II.4.1"
        end
      end
    end

    p = SmallestRootInt(e)

    if p == 2
      return "Thm. II.4.3 (iii)"
    elseif PrimesGAP.isprime(p)
      # (Here we know that 'q mod p <> 1' holds.)
      # Check whether 'p' is a Pierpont prime.
      p = p - 1
      while iseven(p)
        p = div(p, 2)
      end
      while mod(p, 3) == 0
        p = div(p, 3)
      end
      if p == 1
        return "Thm. II.4.3 (ii)"
      end
    end

    # Concerning Remark II.5.4,
    # we need not check \Phi_2 (since m = e if e divides q-1,
    # and we have checked whether m divides q-1),
    # \Phi_4, \Phi_8 (because then m = 2).
    phi6 = bq^2 - q + 1     # this is \Phi_6(q)
    if mod(phi6, e) == 0 ||
       mod(bq^3*(bq^3+1) + 1, e) == 0 ||  # this is \Phi_9(q)
       mod(bq^3*(q-1) + phi6, e) == 0     # this is \Phi_{10}(q)
      return "Rem. II.5.4"
    end

    # We give up.
    return ""
end;

function LoewyLengthCheap(q::Int, n::Int, z::T, m::Int) where {T <: Integer}
    if SufficientCriterionForLoewyBoundAttained(q, n, z, m) != ""
      # The bound is attained.
      return div(n*(q-1), m) + 1
    else
      return 0
    end
end;

function LoewyLengthHard(q::Int, n::Int, z::T, m::Int) where {T <: Integer}
    return LoewyStructureInfo(q, n, z)[:LL]
end;


function OverflowTable(data::Dict)
    if ! haskey(data, :overflowTable)
      monoms = data[ :monomials ]
      d = length(monoms)
      mat = falses(d, d)
      q = data[:parameters][1]
      n = data[:parameters][2]
      for i in 1:d
        for j in 1:(i-1)
          res = all(x -> x < q, monoms[i] + monoms[j])
          mat[i, j] = res
          mat[j, i] = res
        end
        mat[i, i] = all(x -> x < q, monoms[i] + monoms[i])
      end

      data[:overflowTable] = mat
    end

    return data[:overflowTable]
end


function MultTable(data::Dict)
    if ! haskey(data, :multtable)
      monoms = data[:monomials]
      n = length(monoms)
      nn = div(n+1, 2)
      mat = zeros(Int, n, n)
      q = data[:parameters][1]
      if isodd(q)
        qhalf = div(q, 2) + 1
      else
        qhalf = div(q, 2)
      end
      for i in 1:nn
        for j in 1:(i-1)
          if all(x -> x < q, monoms[i] + monoms[j])
            mat[i, j] = i+j-1
            mat[j, i] = i+j-1
          end
        end
        if all(x -> x < qhalf, monoms[i])
          mat[i, i] = i+i-1
        end
      end
      for i in (nn+1):n
        # The [i,j] entry can be nonzero only if i+j <= z+2 holds.
        for j in 1:(n+1-i)
          if all(x -> x < q, monoms[i] + monoms[j])
            mat[i,j] = i+j-1
            mat[j,i] = i+j-1
          end
        end
      end
      data[:multtable] = mat
    end

    return data[:multtable]
end


function OverflowCount(data::Dict)
    if ! haskey(data, :overflowCount)
      mat = OverflowTable(data)
      list = [count(mat[i,:]) for i in 1:size(mat, 1)]
      data[:overflowCount] = list
    end

    return data[:overflowCount]
end


function BasisOfSum(data::Dict, list1::Vector{Int}, list2::Vector{Int})
    return sort(union(list1, list2))
end

function BasisOfIntersection(data::Dict, list1::Vector{Int}, list2::Vector{Int})
    return intersect(list1, list2)
end


function BasisOfProductSpace(data::Dict, list1::Vector{Int}, list2::Vector{Int})
    mat = MultTable(data)
    basis = Int[]

    for i in list1
      for j in list2
        if mat[i,j] != 0
          push!(basis, i+j-1)
        end
      end
    end

    return unique(sort(basis))
end


function BasisOfIdeal(data::Dict, list::Vector{Int})
    return BasisOfProductSpace(data, list, collect(1:length(data[:monomials])))
end


function BasisOfAnnihilator(data::Dict, list::Vector{Int})
    mat = MultTable(data)
    return filter(i -> all(j -> mat[i,j] == 0, list), 1:size(mat, 1))
end;


function BasesOfRadicalSeries(data::Dict)
    lay = data[:layers]
    ll = data[:LL]
    jbylayer = map(i -> findall(isequal(i), lay), 1:(ll-1))
    J = [jbylayer[ll-1]]
    for i in 2:(ll-1)
      push!(J, sort(union(J[i-1], jbylayer[ll-i])))
    end

    return reverse(J)
end

function BasesOfSocleSeries(data::Dict)
    lay = reverse(data[:layers])
    ll = data[:LL]
    sbylayer = map(i -> findall(isequal(i), lay), 0:(ll-2))
    S = [sbylayer[1]]
    for i in 2:(ll-1)
      push!(S, sort(union(S[i-1], sbylayer[i])))
    end

    return S
end


function BasisOfPowers(data::Dict, I::Vector{Int}, p::Int, m::Int)
    result = Int[]
    q = data[:parameters][1]
    pm = BigInt(p)^m
    quo = div(q-1, pm)
    for k in I
      if maximum(data[:monomials][k]) <= quo
        push!(result, pm * (k-1) + 1)
      end
    end

    return result
end


function BasisOfPMRoots(data::Dict, I::Vector{Int}, p::Int, m::Int)
    q= data[:parameters][1]
    pm = BigInt(p)^m
    quo = div(q-1, pm)

    # The first condition means that B_k^{p^m} is zero
    # and hence inside the given subspace.
    # For the second condition, we may assume that B_k^{p^m}
    # is nonzero and hence equal to B_{p^m (k-1)+1}.
    return filter(k -> maximum(data[:monomials][k]) > quo ||
                        pm * (k-1) + 1 in I,
                  1:length(data[:monomials]))
end


IsSubset(I::Vector{Int}, J::Vector{Int}) = all(x -> x in I, J)


function BasisOfPC(data::Dict, I::Vector{Int}, J::Vector{Int})
    # Note that we are interested in subspaces of the radical.
    return filter(k -> IsSubset(J, BasisOfProductSpace(data, [k], I)),
                  2:length(data[:monomials]))
end

