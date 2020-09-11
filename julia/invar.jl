
"""
    rank_boolean_mat(mat::Vector{BitArray{1}})

Return the rank of `mat`, when this is interpreted as a the list of rows
of a matrix over the field with two elements,
where `true` and `false` represent the identity and the zero element of
the field, respectively.
(Two rows can be added by applying `xor.` to them.)
"""
function rank_boolean_mat(mat::Vector{BitArray{1}})
    length(mat) == 0 && return 0
    ncols = length(mat[1])
    nzheads = []
    vectors = []

    for row in mat
      # Reduce the row with the known basis vectors.
      for j in 1:length(vectors)
        if row[nzheads[j]] 
          row = xor.(row, vectors[j])
        end
      end

      j = findnext(row, 1)
      if j != nothing
        # We found a new basis vector.
        push!(vectors, row)
        push!(nzheads, j)
      end
    end

    return length(vectors)
end 


"""
    GrayCodeSwitchIndexIterator(n::Int)

Return an iterator for the sequence of those bit positions in an `n`-bit
Gray code where the flip has to take place.
For large enough `n`, the first values are
1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 5, 1, 2, 1, ...
(This is [series A001511 of the OEIS](https://oeis.org/search?q=A001511).)
"""
mutable struct GrayCodeSwitchIndexIterator
    n::Int
    nn::Int
end

GrayCodeSwitchIndexIterator(n::Int) = GrayCodeSwitchIndexIterator(n, div(n, 2))

# Initialize the iterator, return 1, set the state to [1,0,...,0].
function Base.iterate(itr::GrayCodeSwitchIndexIterator)
    itr.n > 0 || return
    itr.nn > 0 || return (1, Int[])
    next = zeros(Int, itr.nn)
    next[1] = 1
    return (1, next)
end

# Define the iteration step.
# Note that `state` is changed in place.
function Base.iterate(itr::GrayCodeSwitchIndexIterator, state::Vector{Int})
    for i in 1:itr.nn
      val = state[i]
      if val == 0
        state[i] = 1
        return (2*i-1, state)
      elseif val == 1
        state[i] = 2
        return (2*i, state)
      elseif val == 2
        state[i] = 3
        return (2*i-1, state)
      else
        state[i] = 0
      end
    end

    # For odd `n > 1`, we start the second half.
    if mod(itr.n, 2) == 1 && itr.n > 1
      itr.n = 0
      return (2*itr.nn+1, state)
    end

    # The iterator is exhausted.
    return
end


"""
    NumberOfProductsInSubspace(data::Dict, J::Vector{Int}, I::Vector{Int}, bound::Int = 15)

Return either `nothing` or a pair `(e, odd)` of nonnegative big integers
that encodes the number ``2^e \\cdot odd``,
which is the cardinality of the set
``X = \\{ (x, y) \\in U \\times U; x \\cdot y \\in V \\}``,
where `V` and `U` are the subspaces of the Singer algebra described by `data`
that are spanned by the subsets of the canonical basis that are given by
`J` and `I`.

`nothing` is returned whenever the cardinality of the set
``\\{ i \\in I; B_i \\cdot B_j \\not\\in V for some j \\in I \\}``
is larger than `bound`.
"""
function NumberOfProductsInSubspace(data::Dict, J::Vector{Int}, I::Vector{Int}, bound::Int = 15)
    q = data[:parameters][1]
    monom = data[:monomials]
    monV = monom[J]
    monU = monom[I]

    # sums:
    # the nonzero monomials $b \cdot b'$ with $b, b' \in U$
    # such that $b b' \not\in V$;
    # facts:
    # the factorizations of the monomials in 'sums' inside $U \times U$
    # factset:
    # the monomials for the set K (the monomials in U that can occur
    # as factors of nonzero products outside V)
    sums = []
    facts = []
    factset = []
    for m in monU
      for mm in monU
        sum = m + mm
        if maximum(sum) < q && ! (sum in monV)
          push!(facts, [m, mm])
          union!(factset, [m, mm])
          union!(sums, [sum])
        end
      end
    end

    l = length(factset)
    if l == 0
      # all products lie in U
      return [2*size(V, 1), 1]
    elseif l > bound
      # give up
      return
    end

    # outer:
    # the monomials for the set D (those products in S that lie outside V,
    # i. e., for which we have to compute whether they vanish or not)
    outer = setdiff(sums, monV)

    # mats:
    # for each monomial m in outer, a matrix M with
    # M[i,j] = 1 if m = factset[i] * factset[j], and M[i,j] = 0 otherwise.
    mats = [[zeros(Bool, l) for j in 1:l] for i in outer]
    for pair in facts
      pos = findfirst(isequal(pair[1]+pair[2]), outer)
      if pos != nothing
        setindex!(mats[pos][findfirst(isequal(pair[1]), factset)],
                  true, findfirst(isequal(pair[2]), factset))
      end
    end

    # Compute the number of pairs in GF(2)^{|I|} \times GF(2)^{|I|}
    # that belong to elements in the space of (x,y) with x*y inside V.
    ll = length(outer)
    mmats = [[BitArray{1}(m[i]) for m in mats] for i in 1:l]
    two = BigInt(2)
    count = two^l
    mat = [BitArray{1}(zero(m[1])) for m in mats]
    for v in GrayCodeSwitchIndexIterator(l)
      mmats_v = mmats[v]
      mat = [xor.(mmats_v[i], mat[i]) for i in 1:ll]
      count = count + two^(l - rank_boolean_mat(mat))
    end

    # Split the number into 2-part and odd part.
    e = 2 * (length(I) - l)
    while iseven(count)
      e = e + 1
      count = div(count, 2)
    end
    return (e, count)
end

