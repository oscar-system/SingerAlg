#############################################################################
##
#W  mindeg.gi            GAP 4 package SingerAlg                Thomas Breuer
##
##  This file contains the implementation for GAP functions related to
##  the numbers <M>m(q,e)</M>.
##


#############################################################################
##
#M  SingerAlgE( <A> )
#M  SingerAlgE( <q>[, <n>], <z> )
##
InstallMethod( SingerAlgE,
    [ "IsSingerAlgebra" ],
    A -> CallFuncList( SingerAlgE, ParametersOfSingerAlgebra( A ) ) );

InstallMethod( SingerAlgE,
    [ "IsPosInt", "IsPosInt", "IsPosInt" ],
    { q, n, z } -> (q^n-1) / z );

InstallMethod( SingerAlgE,
    [ "IsPosInt", "IsPosInt" ],
    { q, z } -> (q^OrderMod( q, z )-1) / z );


#############################################################################
##
#M  MinimalDegreeOfSingerAlgebraGAP( <A> )
#M  MinimalDegreeOfSingerAlgebraGAP( <q>, <e> )
#M  MinimalDegreeOfSingerAlgebraGAP( <q>, <n>, <e> )
##
##  If the <Ref Attr="LoewyStructureInfo" Label="for a Singer algebra"/>
##  value of the Singer algebra <A>A</A> is known then use it.
##  If the cheap criteria suffice then take their result.
##  Otherwise call the hard method for <A>q</A> and <A>e</A>;
##  if the dimension of the algebra is small then use the algebra
##  also if just the parameters <A>q</A> and <A>e</A> are given.
##
InstallMethod( MinimalDegreeOfSingerAlgebraGAP,
    [ "IsSingerAlgebra and HasLoewyStructureInfoGAP" ],
    A -> LoewyStructureInfoGAP( A ).m );

InstallMethod( MinimalDegreeOfSingerAlgebraGAP,
    [ "IsSingerAlgebra" ],
    function( A )
    local paras, q, n, e;

    # Note that we cannot assume that the parameter 'n' is equal to
    # the multiplicative order of 'q' modulo 'e'.
    paras:= ParametersOfSingerAlgebra( A );
    q:= paras[1];
    n:= paras[2];
    e:= SingerAlgE( A );
    return MinimalDegreeOfSingerAlgebraGAP( q, OrderModExt( q, e, n ), e );
    end );

InstallMethod( MinimalDegreeOfSingerAlgebraGAP,
    [ "IsPosInt", "IsPosInt" ],
    { q, e } -> MinimalDegreeOfSingerAlgebraGAP( q, OrderMod( q, e ), e ) );

InstallMethod( MinimalDegreeOfSingerAlgebraGAP,
    [ "IsPosInt", "IsPosInt", "IsPosInt" ],
    function( q, n, e )
    local e_str, cache_e, m;

    q:= q mod e;
    e_str:= String( e );  # large integers cannot be record component names

    if not IsBound( MinimalDegreeOfSingerAlgebraCache.( e_str ) ) then
      cache_e:= rec();
      MinimalDegreeOfSingerAlgebraCache.( e_str ):= cache_e;
    else
      cache_e:= MinimalDegreeOfSingerAlgebraCache.( e_str );
      if IsBound( cache_e.( q ) ) then
        return cache_e.( q );
      fi;
    fi;

    m:= MinimalDegreeCheapGAP( q, n, e );
    if m = 0 then
      m:= MinimalDegreeHardGAP( q, n, e );
    fi;

    # Extend the cache.
    cache_e.( q ):= m;

    return m;
    end );


#############################################################################
##
#F  MinimalDegreeCheapGAP( <q>, <n>, <e> )
##
##  Return either <C>0</C> or the number <M>m( <A>q</A>, <A>e</A> )</M>,
##  which is the smallest number of powers of <A>q</A>
##  such that <A>e</A> divides the sum of these powers.
##  <P/>
##  The return value <C>0</C> means that the cheap criteria from
##  the two papers <Cite Key="BHHK1"/> and <Cite Key="BHHK2"/>
##  do not suffice to determine <M>m( <A>q</A>, <A>e</A> )</M>.
##  <P/>
##  We assume that
##  <A>q</A> is smaller than <A>e</A>,
##  the argument <A>n</A> is
##  <C>OrderMod( </C><A>q</A><C>, </C><A>e</A><C> )</C>.
##  The function does not use cached values of <M>m( <A>q</A>, <A>e</A> )</M>
##  or values known via the database of low dimensional Singer algebras.
##
InstallGlobalFunction( MinimalDegreeCheapGAP, function( q, n, e )
    local e1, e2, m;

    if e = 1 then
      return 1;
    fi;

    if q = 1 then
      # Example I.6.1.
      return e;
    elif IsEvenInt( n ) and PowerModInt( q, n/2, e ) = e-1 then
      # Decide the case m = 2, see Lemma I.6.3.
      return 2;
    elif n = 2 then
      # Prop. II.2.7.
      e1:= Gcd( e, q-1 );
      e2:= Gcd( e, q+1 );
      if e2 <= e1 or ( IsEvenInt( e ) and IsEvenInt( (q^2-1)/e ) ) then
        return e1;
      else
        return 2 * e1;
      fi;
    fi;

    # Deal with those cases where e is a prime power,
    # and where we know the value of m(q,e).
    if IsPrimePowerInt( e ) then
      # Let e = p^k for a prime p.
      if e mod 2 = 0 then
        # see Prop. II.2.9, note that here we have e > 4 and q mod e <> q-1.
        if q mod 4 = 1 then
          return Gcd( e, q-1 );
        else
          Assert( 1, ( q+1 ) mod e <> 0 );
          return 4;
        fi;
      else
        # see Prop. II.2.10, note that q \equiv 1 \pmod{p}
        # if and only if \gcd( e, q-1 ) \not= 1 holds.
        m:= Gcd( e, q-1 );
        if m <> 1 then
          return m;
        elif n mod 3 = 0 then
          # Here we know that m(q,e) > 2 and thus p \not= 3.
          # Hence \gcd( e, q^{n/3}-1 ) = 1 holds,
          # because otherwise q^{n/3} would be a p-element in (Z/eZ)^*.
          # Thus Lemma I.6.4 yields m(q,e) \leq 3.
          # (This is a generalization of Cor. II.2.11.)
          return 3;
        elif 2 * n = Phi( e ) then
          # Cor. II.2.14.
          return 3;
        elif e mod 11 = 0 then
          # Prop. II.2.16.
          return 5;
        fi;
      fi;
    fi;

    # Deal with those cases where e is twice an odd prime power,
    # and where we know the value of m(q,e).
    if IsEvenInt( e ) and IsPrimePowerInt( e/2 ) then
      # see Prop. II.2.17, note that the order of q mod e is a power of p
      # if and only if \gcd( e/2, q-1 ) \not= 1 holds.
      m:= Gcd( e, q-1 );
      if m > 2 then
        return m;
      fi;
    fi;

    # We give up.
    return 0;
    end );


#############################################################################
##
#F  VectorIterator( m, n, s )
##
##  iterator for all vectors of length n
##  with entries in { 0, 1, 2, ... m }
##  and coefficient sum s, such that ...
##
##  fill the first n positions in v with smallest partition of s into
##  numbers at most m
##  (we assume that s <= m*n)
##
BindGlobal( "VectorIterator_ResetPrefix", function( v, m, n, s )
    local rest, i, j;

    rest:= s;
    i:= 1;
    while m < rest do
      v[i]:= m;
      rest:= rest - m;
      i:= i + 1;
    od;
    v[i]:= rest;
    for j in [ i+1 .. n ] do
      v[j]:= 0;
    od;

    return v;
    end );

BindGlobal( "VectorIterator", function( m, n, s )
    local next;

    if s > m*n then
      # The iterator is empty.
      next:= false;
    else
      # Initialize the coefficient vector.
      next:= VectorIterator_ResetPrefix( [], m, n, s );
    fi;

    return IteratorByFunctions( rec(
             NextIterator:= function( iter )
               local result, pos, succ;

               result:= iter!.next;
               # Find the first position with a nonzero value
               # such that the value on the right is smaller than m.
               pos:= First( [ 1 .. iter!.n - 1 ],
                            i -> result[i] <> 0 and result[ i+1 ] < iter!.m );
               if pos = fail then
                 succ:= false;
               else
                 succ:= ShallowCopy( result );
                 succ[ pos ]:= result[ pos ] - 1;
                 succ[ pos + 1 ]:= result[ pos + 1 ] + 1;
                 VectorIterator_ResetPrefix( succ, iter!.m, pos,
                                         Sum( succ{ [ 1 .. pos ] } ) );
               fi;
               iter!.next:= succ;
               return result;
             end,

             IsDoneIterator:= iter -> ( iter!.next = false ),

             ShallowCopy:= function( iter )
             end,

             m:= m,
             n:= n,
             s:= s,

             next:= next,
           ) );
    end );


#############################################################################
##
#F  MinimalDegreeHardGAP( <q>, <n>, <e> )
##
##  - assume that the cheap criteria do not yield the result
##    (e.g., m = 2 cannot occur here)
##  - assume q < e
##  - assume that n equals OrderMod( q, e )
##  - do not use the cache
##  - do not create the monomials by computing all q-adic expansions at once,
##    but create these coefficient vectors one by one, using an iterator
##  - for increasing values of m, run over those coefficient vectors
##    of length n and with entries in \{ 0, 1, \ldots, m \}
##    such that the coefficient sum is s
##    and such that a largest entry is in the first position.
##
InstallGlobalFunction( MinimalDegreeHardGAP, function( q, n, e )
    local z, e1, m, powers, a, v;

    # If A[q,n,z] has small dimension and e is not very small
    # then enumerate its basis.
    # (For small e, we known that the m values are small,
    # so it is cheaper not to enumerate.)
    if e > 100 then
      z:= ( q^n - 1 ) / e;
      if z <= 10^5 then
        return LoewyStructureInfoGAP( q, n, z ).m;
      fi;
    fi;

    # m(q,e) is divisible by \gcd( e, q-1 ), and m(q,e) \geq 3.
    e1:= Gcd( e, q-1 );
    if e1 = 1 then
      m:= 3;
    elif e1 = 2 then
      m:= 4;
    else
      m:= e1;
    fi;

    powers:= List( [ 1 .. n-1 ], i -> PowerModInt( q, i, e ) );

    while true do
      for a in [ 1 .. m ] do
        for v in VectorIterator( a, n-1, m-a ) do
          if ( a + v * powers ) mod e = 0 then
            return m;
          fi;
        od;
      od;
      m:= m + e1;
    od;
    end );


#############################################################################
##
#M  MinimalDegreeOfSingerAlgebraJulia( <A> )
#M  MinimalDegreeOfSingerAlgebraJulia( <q>, <e> )
#M  MinimalDegreeOfSingerAlgebraJulia( <q>, <n>, <e> )
##
##  Use the same criteria as for the &GAP; variant.
##  These methods are available only if &Julia; is available.
##
if IsPackageMarkedForLoading( "JuliaInterface", "" ) then

InstallMethod( MinimalDegreeOfSingerAlgebraJulia,
    [ "IsSingerAlgebra and HasLoewyStructureInfoJulia" ],
    A -> JuliaToGAP( IsInt,
             Julia.Base.get( LoewyStructureInfoJulia( A ),
                             JuliaSymbol( "m" ), 0 )  ));

InstallMethod( MinimalDegreeOfSingerAlgebraJulia,
    [ "IsSingerAlgebra" ],
    function( A )
    local paras, q, n, e;

    # Note that we cannot assume that the parameter 'n' is equal to
    # the multiplicative order of 'q' modulo 'e'.
    paras:= ParametersOfSingerAlgebra( A );
    q:= paras[1];
    n:= paras[2];
    e:= SingerAlgE( A );

    # Supply 'A' as an argument in order to set the GAP attribute.
    return JuliaToGAP( IsInt,
               Julia.SingerAlg.MinimalDegree( q, OrderModExt( q, e, n ), e,
                                              A ) );
    end );

InstallMethod( MinimalDegreeOfSingerAlgebraJulia,
    [ "IsPosInt", "IsPosInt" ],
    { q, e } -> JuliaToGAP( IsInt,
                    Julia.SingerAlg.MinimalDegree( q, GAPToJulia( e ) ) ) );

InstallMethod( MinimalDegreeOfSingerAlgebraJulia,
    [ "IsPosInt", "IsPosInt", "IsPosInt" ],
    { q, n, e } -> JuliaToGAP( IsInt,
               Julia.SingerAlg.MinimalDegree( q, n, GAPToJulia( e ) ) ) );

fi;


#############################################################################
##
#E

