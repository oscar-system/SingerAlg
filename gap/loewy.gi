#############################################################################
##
#W  loewy.gi             GAP 4 package SingerAlg                Thomas Breuer
##
##  This file contains implementations of GAP functions for studying
##  the Loewy structure of Singer algebras A[q,z].
##


##  for those who worked with an earlier version of the package ...
LoewyLayersData:= function( arg )
    Print( "use LoewyStructureInfo not LoewyLayersData\n" );
    return CallFuncList( LoewyStructureInfoGAP, arg );
    end;


if IsPackageMarkedForLoading( "JuliaInterface", "" ) then

#############################################################################
##
#M  LoewyStructureInfoJulia( <A> )
##
##  Call a Julia function, convert its result to a record,
##  but keep the record components in Julia.
##
InstallMethod( LoewyStructureInfoJulia,
    [ "IsSingerAlgebra" ],
    A -> CallFuncList( LoewyStructureInfoJulia,
                       ParametersOfSingerAlgebra( A ) ) );

InstallMethod( LoewyStructureInfoJulia,
    [ "IsPosInt", "IsPosInt" ],
    { q, z } -> LoewyStructureInfoJulia( q, OrderMod( q, z ), z ) );

InstallMethod( LoewyStructureInfoJulia,
    [ "IsPosInt", "IsPosInt", "IsPosInt" ],
    { q, n, z } -> CallFuncList( Julia.SingerAlg.LoewyStructureInfo,
                       List( [ q, n, z ], GAPToJulia ) ) );

fi;


if IsPackageMarkedForLoading( "JuliaInterface", "" ) then

#############################################################################
##
#M  LoewyStructureInfo( <A> )
##
##  We prefer 'LoewyStructureInfoJulia' to 'LoewyStructureInfoGAP'.
##  For that, we convert the Julia dictionary into a GAP record.
##
InstallMethod( LoewyStructureInfo,
    [ "IsSingerAlgebra" ],
    A -> JuliaToGAP( IsRecord, LoewyStructureInfoJulia( A ), true ) );

InstallMethod( LoewyStructureInfo,
    [ "IsPosInt", "IsPosInt" ],
    { q, z } -> LoewyStructureInfo( q, OrderMod( q, z ), z ) );

InstallMethod( LoewyStructureInfo,
    [ "IsPosInt", "IsPosInt", "IsPosInt" ],
    { q, n, z } -> JuliaToGAP( IsRecord, LoewyStructureInfoJulia( q, n, z ),
                               true ) );

fi;


#############################################################################
##
#F  LoewyStructureInfoGAP( <A> )
#F  LoewyStructureInfoGAP( <q>[, <n>], <z> )
##
InstallMethod( LoewyStructureInfoGAP,
    [ "IsSingerAlgebra" ],
    A -> CallFuncList( LoewyStructureInfoGAP,
                       ParametersOfSingerAlgebra( A ) ) );

InstallMethod( LoewyStructureInfoGAP,
    [ "IsPosInt", "IsPosInt" ],
    { q, z } -> LoewyStructureInfoGAP( q, OrderMod( q, z ), z ) );

InstallMethod( LoewyStructureInfoGAP,
    [ "IsPosInt", "IsPosInt", "IsPosInt" ],
    function( q, n, z )
    local islessorequal, monomials, layers, degrees, predecessors, m, i,
          mon, lambda, pred, j, mm;

    if n mod OrderMod( q, z ) <> 0 then
      Error( "<z> must divide <q>^<n> - 1" );
    fi;

    islessorequal:= function( mon1, mon2 )
      local i;

      for i in [ 1 .. n ] do
        if mon2[i] < mon1[i] then
          return false;
        fi;
      od;
      return true;
    end;

    monomials:= [ 0 * [ 1 .. n ] ];
    layers:= [ 0 ];
    degrees:= [ 0 ];    # just for speedup: avoid comparisons

    predecessors:= [ 0 ];

    m:= n * (q-1);
    for i in [ 1 .. z ] do
      mon:= CoefficientsQadicReversed( i, z, q, n );
      lambda:= 0;
      pred:= 1;
      mm:= Sum( mon );
      for j in [ 2 .. i ] do
        if lambda < layers[j] and degrees[j] < mm
                              and islessorequal( monomials[j], mon ) then
          lambda:= layers[j];
          pred:= j;
        fi;
      od;
      monomials[i+1]:= mon;
      layers[i+1]:= lambda + 1;
      degrees[i+1]:= mm;
      predecessors[i+1]:= pred;

      if lambda = 0 then
        if mm < m then
          m:= mm;
        fi;
      fi;
    od;

    # Extract information about one longest chain.
    i:= Length( monomials );
    pred:= [];
    while i > 0 do
      Add( pred, i );
      i:= predecessors[i];
    od;

    return rec( monomials:= monomials,
                layers:= layers,
                chain:= pred,
                m:= m,
                LL:= lambda + 2,
                parameters:= [ q, n, z ] );
    end );


#############################################################################
##
#F  DimensionsLoewyFactorsGAP( <A> )
#F  DimensionsLoewyFactorsJulia( <A> )
##
##  Return the GAP object or the Julia object, respectively.
##
BindGlobal( "DimensionsLoewyFactorsGAP", function( A )
    local data, v, i;

    data:= LoewyStructureInfoGAP( A );
    v:= ListWithIdenticalEntries( data.LL, 0 );
    for i in data.layers do
      v[ i+1 ]:= v[ i+1 ] + 1;
    od;

    return v;
    end );

if IsPackageMarkedForLoading( "JuliaInterface", "" ) then

BindGlobal( "DimensionsLoewyFactorsJulia",
    A -> Julia.SingerAlg.LoewyVector( LoewyStructureInfoJulia( A ) ) );

fi;


#############################################################################
##
#M  DimensionsLoewyFactors( <A> )
##
##  If Julia is available then use the Julia functionality,
##  otherwise use the GAP code.
##
InstallMethod( DimensionsLoewyFactors,
    [ "IsSingerAlgebra" ],
    A -> DimensionsLoewyFactorsGAP( A ) );


if IsPackageMarkedForLoading( "JuliaInterface", "" ) then

InstallMethod( DimensionsLoewyFactors,
    [ "IsSingerAlgebra" ],
    A -> JuliaToGAP( IsList, DimensionsLoewyFactorsJulia( A ), true ) );

fi;


#############################################################################
##
#F  LoewyVectorAbbreviated( <v> )
#F  LoewyVectorExpanded( <v> )
##
InstallGlobalFunction( LoewyVectorAbbreviated, function( v )
    local result, x, m, i;

    if not IsDenseList( v ) then
      Error( "<v> must be a dense list" );
    elif Length( v ) = 0 then
      return [];
    fi;
    result:= [];
    x:= v[1];
    m:= 1;
    for i in [ 2 .. Length( v ) ] do
      if v[i] = x then
        m:= m + 1;
      elif m = 1 then
        Add( result, x );
        x:= v[i];
      else
        Add( result, [ x, m ] );
        x:= v[i];
        m:= 1;
      fi;
    od;

    if m = 1 then
      Add( result, x );
    else
      Add( result, [ x, m ] );
    fi;

    return result;
    end );

InstallGlobalFunction( LoewyVectorExpanded, function( v )
    local result, x, i;

    if not IsDenseList( v ) then
      Error( "<v> must be a dense list" );
    fi;
    result:= [];
    for x in v do
      if IsList( x ) then
        for i in [ 1 .. x[2] ] do
          Add( result, x[1] );
        od;
      else
        Add( result, x );
      fi;
    od;

    return result;
    end );


#############################################################################
##
#F  SingerAlgebra( <q>[, <n>], <z>[, <R>] )
#F  SingerAlgebra( <arec>[, <R>] )
##
##  The algebra returned by this function claims to be a structure constants
##  algebra (via the filter 'IsSCAlgebraObjCollection'),
##  but the structure constants table is not created until one asks for
##  something that needs it.
##  Currently a special method for 'CanonicalBasis' makes sure that the
##  relevant data get stored.
##
InstallGlobalFunction( SingerAlgebra, function( arg )
    local arec, paras, q, n, e, R, z, zero, filter, Fam, A;

    if Length( arg ) = 1 and IsRecord( arg[1] ) then
      # The record is assumed to belong to the database of Singer algebras.
      arec:= arg[1];
      q:= arec.q;
      n:= arec.n;
      if IsBound( arec.z ) then
        z:= arec.z;
      elif IsBound( arec.e ) then
        z:= ( q^n - 1 ) / arec.e;
      else
        Error( "<arec> must contain one of the components z or e" );
      fi;
      R:= Rationals;
    elif Length( arg ) = 2 and IsRecord( arg[1] ) and IsRing( arg[2] ) then
      # The record is assumed to belong to the database of Singer algebras.
      arec:= arg[1];
      q:= arec.q;
      n:= arec.n;
      if IsBound( arec.z ) then
        z:= arec.z;
      elif IsBound( arec.e ) then
        z:= ( q^n - 1 ) / arec.e;
      else
        Error( "<arec> must contain one of the components z or e" );
      fi;
      R:= arg[2];
    elif Length( arg ) = 1 and IsList( arg[1] ) then
      # The parameters must be q, n, e[, R].
      paras:= arg[1];
      if Length( paras ) = 3 then
        q:= paras[1];
        n:= paras[2];
        e:= paras[3];
        R:= Rationals;
      elif Length( paras ) = 4 then
        q:= paras[1];
        n:= paras[2];
        e:= paras[3];
        R:= paras[4];
      else
        Error( "usage: SingerAlgebra( <list> ) with list of length 3 or 4" );
      fi;
      z:= ( q^n - 1 ) / e;
    elif Length( arg ) = 2 then
      # The parameters must be q, z.
      q:= arg[1];
      z:= arg[2];
      n:= OrderMod( q, z );
      R:= Rationals;
    elif Length( arg ) = 3 and IsInt( arg[3] ) then
      # The parameters must be q, n, z.
      q:= arg[1];
      n:= arg[2];
      z:= arg[3];
      R:= Rationals;
    elif Length( arg ) = 3 and IsRing( arg[3] ) then
      # The parameters must be q, z, R.
      q:= arg[1];
      z:= arg[2];
      R:= arg[3];
      n:= OrderMod( q, z );
    elif Length( arg ) = 4 then
      # The parameters must be q, n, z, R.
      q:= arg[1];
      n:= arg[2];
      z:= arg[3];
      R:= arg[4];
    else
      Error( "usage: SingerAlgebra( <arec>[, <R>] ) or ",
             "SingerAlgebra( <q>[, <n>], <z>[, <R>] ) or ",
             "SingerAlgebra( <list> )" );
    fi;

    if not ( IsPosInt( q ) and IsPosInt( n ) and IsPosInt( z ) ) then
      Error( "<q>, <n>, <z> must be positive integers" );
    elif z <> 1 and PowerMod( q, n, z ) <> 1 then
      Error( "<z> must divide <q>^<n> - 1" );
    elif q = 1 then
      Error( "<q> must be an integer > 1" );
    fi;

    # Create the algebra as far as necessary.
    # (Do not set a 'Name' value, in order to use the different methods
    # installed for 'ViewObj' and 'PrintObj'.)
    zero := Zero( R );
    filter:= IsSCAlgebraObj;
    if IsAdditivelyCommutativeElementFamily( FamilyObj( zero ) ) then
      filter:= filter and IsAdditivelyCommutativeElement;
    fi;
    Fam:= NewFamily( "SCAlgebraObjFamily", filter );
    if Zero( ElementsFamily( FamilyObj( R ) ) ) <> fail then
      SetFilterObj( Fam, IsFamilyOverFullCoefficientsFamily );
    else
      Fam!.coefficientsDomain:= R;
    fi;
    Fam!.zerocoeff := zero;
    SetCharacteristic( Fam, Characteristic( R ) );
    SetCoefficientsFamily( Fam, ElementsFamily( FamilyObj( R ) ) );

    A:= Objectify( NewType( CollectionsFamily( Fam ),
                   IsSingerAlgebra and IsAttributeStoringRep ),
                   rec() );
    SetLeftActingDomain( A, R );
    SetParametersOfSingerAlgebra( A, [ q, n, z ] );
    SetGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( A, [ 1 .. z+1 ] );
    SetDimension( A, z+1 );
    Fam!.fullSCAlgebra:= A;
    SetIsFullSCAlgebra( A, true );

    return A;
    end );


#############################################################################
##
#M  Display( <A> )
#M  ViewObj( <A> )
#M  PrintObj( <A> )
#M  DisplayString( <A> )
#M  ViewString( <A> )
#M  PrintString( <A> )
#M  String( <A> )
##
##  According to the Section "View and Print" in the GAP Reference Manual,
##  we need methods (only) for
##  'String' (which then covers 'PrintString' and 'PrintObj') and
##  'ViewString' (which then covers 'ViewObj').
##  (We do not need 'DisplayString', which would cover 'Display'.)
##
##  We want to *view* Singer algebras as 'A[q,n,z[,R]]',
##  and to *print* them as calls to 'SingerAlgebra'.
##
InstallMethod( ViewString,
    [ "IsSingerAlgebra" ],
    function( A )
    local paras;

    paras:= ParametersOfSingerAlgebra( A );
    if LeftActingDomain( A ) = Rationals then
      return Concatenation( "A[",
                 JoinStringsWithSeparator( List( paras, String ), "," ),
                 "]" );
    else
      return Concatenation( "A[",
                 JoinStringsWithSeparator( List( paras, String ), "," ),
                 ",", String( LeftActingDomain( A ) ), "]" );
    fi;
    end );

InstallMethod( String,
    [ "IsSingerAlgebra" ],
    function( A )
    local paras;

    paras:= ParametersOfSingerAlgebra( A );
    if LeftActingDomain( A ) = Rationals then
      return Concatenation( "SingerAlgebra( ",
                 JoinStringsWithSeparator( List( paras, String ), ", " ),
                 " )" );
    else
      return Concatenation( "SingerAlgebra( ",
                 JoinStringsWithSeparator( List( paras, String ), ", " ),
                 ", ", String( LeftActingDomain( A ) ), " )" );
    fi;
    end );

#T Currently the above installations are unfortunately *not* enough,
#T since some 'ViewObj' and 'PrintObj' methods for algebras interfere,
#T as well as a 'PrintString' method for magmas.

InstallMethod( ViewObj,
    [ "IsSingerAlgebra" ],
    function( A ) Print( ViewString( A ) ); end );

InstallMethod( PrintObj,
    [ "IsSingerAlgebra" ],
    function( A ) Print( PrintString( A ) ); end );

InstallMethod( PrintString,
    [ "IsSingerAlgebra" ],
    String );


#############################################################################
##
#M  CanonicalBasis( <A> )
##
##  This method provides the internal data for treating the Singer algebra
##  <A> as a structure constants algebra in GAP.
##
##  Formally, we require those filters that are required also by the
##  GAP library method for full s.c. algebras,
##  in order to guarantee a higher rank for our method;
##  these filters are set in algebras returned by 'SingerAlgebra'.
##
InstallMethod( CanonicalBasis,
    [ Concatenation( "IsSingerAlgebra and IsFreeLeftModule and ",
                     "IsSCAlgebraObjCollection and IsFullSCAlgebra" ) ],
    function( A )
    local paras, q, n, z, dim, coeffs, T, R, zero, one, empty, nonempty,
          i, j, Fam, gens;

    # Create the structure constants table.
    paras:= ParametersOfSingerAlgebra( A );
    q:= paras[1];
    n:= paras[2];
    z:= paras[3];
    dim:= Dimension( A );
    coeffs:= List( [ 0 .. z ],
                   k -> CoefficientsQadicReversed( k, z, q, n ) );
    T:= [];
    R:= LeftActingDomain( A );
    zero:= Zero( R );
    one:= One( R );
    empty:= MakeImmutable( [ [], [] ] );
    nonempty:= List( [ 1 .. dim ],
                     i -> MakeImmutable( [ [ i ], [ one ] ] ) );
    for i in [ 1 .. dim ] do
      T[i]:= [];
      for j in [ 1 .. i-1 ] do
        T[i][j]:= T[j][i];
      od;
      for j in [ i .. dim-i+1 ] do
        if q <= MaximumList( coeffs[i] + coeffs[j], 0 ) then
          T[i][j]:= empty;
        else
          T[i][j]:= nonempty[ i+j-1 ];
        fi;
      od;
      for j in [ dim-i+2 .. dim ] do
        T[i][j]:= empty;
      od;
    od;
    T[ dim+1 ]:= 1;  # commutativity flag
    T[ dim+2 ]:= zero;

    # Set the necessary entries in the family.
    Fam:= ElementsFamily( FamilyObj( A ) );
    Fam!.sctable:= T;
    Fam!.names:= MakeImmutable( List( [ 0 .. dim-1 ],
                     i -> Concatenation( "b", String( i ) ) ) );
    Fam!.zerocoeff:= zero;
    Fam!.defaultTypeDenseCoeffVectorRep :=
        NewType( Fam, IsSCAlgebraObj and IsDenseCoeffVectorRep );
    SetZero( Fam, ObjByExtRep( Fam, ListWithIdenticalEntries( dim, zero ) ) );

    # Set the algebra generators.
    gens:= MakeImmutable( List( IdentityMat( dim, R ),
               x -> ObjByExtRep( Fam, x ) ) );
    SetGeneratorsOfAlgebra( A, gens );
    SetGeneratorsOfAlgebraWithOne( A, gens );
    SetOne( A, gens[1] );
    Fam!.basisVectors:= gens;

    # Delegate to the library method for full s.c. algebras,
    # which has a lower rank.
    TryNextMethod();
    end );


#############################################################################
##
#M  Representative( <A> )
#M  GeneratorsOfAlgebra( <A> )
#M  GeneratorsOfAlgebraWithOne( <A> )
##
##  Note that we cannot use 'RedispatchOnCondition' here,
##  because we have only the attribute tester 'HasCanonicalBasis'
##  that may be missing,
##  whereas 'RedispatchOnCondition' checks for missing property values
##  plus their testers.
##
InstallMethod( Representative,
    [ "IsSingerAlgebra" ],
    function( A )
    if HasCanonicalBasis( A ) then
      TryNextMethod();
    fi;

    # Set the necessary data and redispatch.
    CanonicalBasis( A );
    return Representative( A );
    end );


InstallMethod( GeneratorsOfAlgebra,
    [ "IsSingerAlgebra" ],
    function( A )
    if HasCanonicalBasis( A ) then
      TryNextMethod();
    fi;

    # Set the necessary data and redispatch.
    CanonicalBasis( A );
    return GeneratorsOfAlgebra( A );
    end );


InstallMethod( GeneratorsOfAlgebraWithOne,
    [ "IsSingerAlgebra" ],
    function( A )
    if HasCanonicalBasis( A ) then
      TryNextMethod();
    fi;

    # Set the necessary data and redispatch.
    CanonicalBasis( A );
    return GeneratorsOfAlgebraWithOne( A );
    end );


#############################################################################
##
#M  GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( <triv> )
##
##  Provide a method for the trivial subspace/subalgebra of a Singer algebra.
##  (We cannot be sure that this attribute gets set on creation.)
##
InstallMethod( GeneratingSubsetOfCanonicalBasisOfSingerAlgebra,
    [ "IsVectorSpace and HasParent" ],
    function( V )
    if IsSingerAlgebra( Parent( V ) ) and IsTrivial( V ) then
      return [];
    fi;
    TryNextMethod();
    end );


#############################################################################
##
#M  LoewyLengthGAP( <A> )
#M  LoewyLengthGAP( <q>, <z> )
#M  LoewyLengthGAP( <q>, <n>, <z>, <m> )
##
##  Use stored 'LoewyStructureInfoGAP' values,
##  use the cheap criteria when the upper bound is attained (using <m>),
##  compute 'LoewyStructureInfoGAP' only if necessary.
##
InstallMethod( LoewyLengthGAP,
    [ "IsSingerAlgebra and HasLoewyStructureInfoGAP" ],
    A -> LoewyStructureInfoGAP( A ).LL );

InstallMethod( LoewyLengthGAP,
    [ "IsSingerAlgebra" ],
    function( A )
    local m, paras;

    # We need m(q,e) anyhow in order to compute the upper bound
    # to which the cheap criteria refer.
    m:= MinimalDegreeOfSingerAlgebraGAP( A );
    if HasLoewyStructureInfoGAP( A ) then
      # Perhaps the computation of 'm' has triggered this.
      return LoewyStructureInfoGAP( A ).LL;
    else
      paras:= ParametersOfSingerAlgebra( A );
      return LoewyLengthGAP( paras[1], paras[2], paras[3], m );
    fi;
    end );

InstallMethod( LoewyLengthGAP,
    [ "IsPosInt", "IsPosInt" ],
    function( q, z )
    local n;

    n:= OrderMod( q, z );
    return LoewyLengthGAP( q, n, z,
               MinimalDegreeOfSingerAlgebraGAP( q, SingerAlgE( q, n, z ) ) );
    end );

InstallMethod( LoewyLengthGAP,
    [ "IsPosInt", "IsPosInt", "IsPosInt", "IsPosInt" ],
    function( q, n, z, m )
    local l;

    l:= LoewyLengthCheapGAP( q, n, z, m );
    if l = 0 then
      l:= LoewyLengthHardGAP( q, n, z, m );
    fi;
    return l;
    end );


#############################################################################
##
#F  OrderModExt( <n>, <m>[, <bound>] )
##
##  The optional argument <bound> is not yet supported for 'OrderMod'
##  in GAP 4.11.0, but it is useful in the context of this package.
##
InstallGlobalFunction( OrderModExt, function( n, m, bound... )
    local  x, o, d;

    # check the arguments and reduce $n$ into the range $0..m-1$
    if m <= 0  then Error("<m> must be positive");  fi;
    if n < 0   then n := n mod m + m;  fi;
    if m <= n  then n := n mod m;      fi;

    # return 0 if $m$ is not coprime to $n$
    if GcdInt(m,n) <> 1  then
        o := 0;

    # compute the order simply by iterated multiplying, $x= n^o$ mod $m$
    elif m < 100  then
        x := n;  o := 1;
        while x > 1  do
            x := x * n mod m;  o := o + 1;
        od;

    # otherwise try the divisors of $\lambda(m)$ and their divisors, etc.
    else
        if Length( bound ) = 1 then
            # We know a multiple of the desired order.
            o := bound[1];
        else
            # The default a priori known multiple is 'Lambda( m )'.
            o := Lambda( m );
        fi;
        for d in PrimeDivisors( o ) do
            while o mod d = 0  and PowerModInt(n,o/d,m) = 1  do
                o := o / d;
            od;
        od;

    fi;

    return o;
    end );


#############################################################################
##
#F  SufficientCriterionForLoewyBoundAttained( <q>, <n>, <z>, <m> )
##
InstallGlobalFunction( SufficientCriterionForLoewyBoundAttained,
    function( q, n, z, m )
    local qm, e, r, p, sum, d, phi6;

    if n <= 3 then
      return "Cor. I.7.1 (n <= 3)";
    elif z < 70 then
      # The bound is attained, by the classif. of small dim. Singer alg.
      return "z < 70";
    elif m = 2 then
      return "La. I.6.3";
    elif ( q - 1 ) mod m = 0 then
      return "Thm. I.7.1";
    elif n * (q-1) < 3 * m then
      return "La. I.7.1 (iii)";
    fi;

    qm:= q mod m;
    if 1 < qm and Int( n * ( qm-1 ) / m ) = 1 then
      return "Prop. II.3.15";
    fi;

    e:= ( q^n - 1 ) / z;

    if e <= 32 then
      return "Prop. II.6.1 (e <= 32)";
    elif e <= 3 * m then
      return "Prop. II.6.4";
    elif n <= 5 and ((q^n-1)/(q-1)) mod e = 0 then
      return "Prop. II.5.3, II.5.6 (e | (q^n-1)/(q-1), n <= 5)";
    elif n mod ( m / Gcd( m, q-1 ) * OrderModExt( q, e, n ) ) = 0 then
      return "La. II.5.2 (ii)";
    fi;

    if ( (q^n-1)/(q-1) ) mod e = 0 then
      # we know m <= n
      if m >= n-1 then
        return "Prop. II.5.1 (ii)";
      elif  m = n-2 and ( q mod m ) in [ 1 .. Int( (m+1)/2 ) ] then
        return "Prop. II.5.1 (iii)";
      elif Int( (n-m) * (q-1) / m ) mod ( n-m ) = 0 then
        return "Prop. II.5.1 (i)";
      fi;
    fi;

    if ( n mod m = 0 ) then
      if Sum( List( [ 0 .. m-1 ], i -> q^(i*n/m) ) ) mod e = 0 then
        return "La. II.4.1 for r = 1";
      fi;
      for r in DivisorsInt( n ) do
        if (n/r) mod m = 0 and
           Sum( [ 0 .. m-1 ], i -> q^(n*i/(m*r)) ) mod e = 0 then
          return "La. II.4.1";
        fi;
      od;
    fi;

    p:= SmallestRootInt( e );

    if p = 2 then
      return "Thm. II.4.3 (iii)";
    elif IsPrimeInt( p ) then
      # (Here we know that 'q mod p <> 1' holds.)
      # Check whether 'p' is a Pierpont prime.
      p:= p - 1;
      while p mod 2 = 0 do
        p:= p/2;
      od;
      while p mod 3 = 0 do
        p:= p/3;
      od;
      if p = 1 then
        return "Thm. II.4.3 (ii)";
      fi;
    fi;

    # Concerning Remark II.5.4,
    # we need not check \Phi_2 (since m = e if e divides q-1,
    # and we have checked whether m divides q-1),
    # \Phi_4, \Phi_8 (because then m = 2).
    phi6:= q^2 - q + 1;    # this is \Phi_6(q)
    if phi6 mod e = 0 or
       ( q^3*(q^3+1) + 1 ) mod e = 0 or     # this is \Phi_9(q)
       ( q^3*(q-1) + phi6 ) mod e = 0 then  # this is \Phi_{10}(q)
      return "Rem. II.5.4";
    fi;

    # We give up.
    return "";
    end );

InstallGlobalFunction( LoewyLengthCheapGAP, function( q, n, z, m )
    if SufficientCriterionForLoewyBoundAttained( q, n, z, m ) <> "" then
      return Int( n * ( q - 1 ) / m ) + 1;
    else
      return 0;
    fi;
    end );

InstallGlobalFunction( LoewyLengthHardGAP,
    { q, n, z, m } -> LoewyStructureInfoGAP( q, n, z ).LL );


#############################################################################
##
#M  LoewyLengthJulia( <A> )
#M  LoewyLengthJulia( <q>, <z> )
#M  LoewyLengthJulia( <q>, <n>, <z>, <m> )
##
##  Use the same criteria as for the &GAP; variant.
##  These methods are available only if &Julia; is available.
##
if IsPackageMarkedForLoading( "JuliaInterface", "" ) then

InstallMethod( LoewyLengthJulia,
    [ "IsSingerAlgebra and HasLoewyStructureInfoJulia" ],
    A -> JuliaToGAP( IsInt,
             Julia.Base.get( LoewyStructureInfoJulia( A ),
                             JuliaSymbol( "LL" ), 0 )  ));

InstallMethod( LoewyLengthJulia,
    [ "IsSingerAlgebra" ],
    function( A )
    local paras;

    paras:= ParametersOfSingerAlgebra( A );

    # Leave the computation of m(q,e) and of the Loewy length to Julia,
    # let Julia set also the 'MinimalDegreeOfSingerAlgebraJulia' value in A.
    return JuliaToGAP( IsInt,
               Julia.SingerAlg.LoewyLength( paras[1], paras[2], paras[3],
                   A ) );
    end );

InstallMethod( LoewyLengthJulia,
    [ "IsPosInt", "IsPosInt" ],
    function( q, z )
    return JuliaToGAP( IsInt, Julia.SingerAlg.LoewyLength( q, z ) );
    end );

InstallMethod( LoewyLengthJulia,
    [ "IsPosInt", "IsPosInt", "IsPosInt", "IsPosInt" ],
    function( q, n, z, m )
    return JuliaToGAP( IsInt,
               Julia.SingerAlg.LoewyLength( q, n, z, m ) );
    end );

fi;


#############################################################################
##
#M  RadicalOfAlgebra( <A> )
##
##  The Jacobson radical of a Singer algebra of dimension <M>z+1</M>
##  has the basis <M>[ b_1, b_2, \ldots, b_z ]</M>.
##
InstallMethod( RadicalOfAlgebra,
    [ "IsSingerAlgebra" ],
    function( A )
    local B, range, S;

    B:= BasisVectors( CanonicalBasis( A ) );
    range:= [ 2 .. Length( B ) ];
    S:= SubalgebraNC( A, B{ range }, "basis" );
    SetGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( S, range );

    return S;
    end );


#############################################################################
##
#M  RadicalSeriesOfAlgebra( <A> )
##
InstallMethod( RadicalSeriesOfAlgebra,
    [ "IsSingerAlgebra" ],
    function( A )
    local result, data, B, i, S;

    result:= [ A, RadicalOfAlgebra( A ) ];
    data:= LoewyStructureInfoGAP( A );
    data:= SingerAlg.BasesOfRadicalSeries( data );
    B:= BasisVectors( CanonicalBasis( A ) );
    for i in [ 2 .. Length( data ) ] do
      S:= SubalgebraNC( A, B{ data[i] }, "basis" );
      SetGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( S, data[i] );
      result[i+1]:= S;
    od;
    Add( result, TrivialSubalgebra( A ) );

    return result;
    end );


#############################################################################
##
#M  SocleSeriesOfAlgebra( <A> )
##
InstallMethod( SocleSeriesOfAlgebra,
    [ "IsSingerAlgebra" ],
    function( A )
    local result, data, B, i, S;

    result:= [ TrivialSubalgebra( A ) ];
    data:= LoewyStructureInfoGAP( A );
    data:= SingerAlg.BasesOfSocleSeries( data );
    B:= BasisVectors( CanonicalBasis( A ) );
    for i in [ 1 .. Length( data ) ] do
      S:= SubalgebraNC( A, B{ data[i] }, "basis" );
      SetGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( S, data[i] );
      result[i+1]:= S;
    od;
    Add( result, A );

    return result;
    end );


#############################################################################
##
#M  Intersection2( <A>, <B> )
##
InstallMethod( Intersection2,
    IsIdenticalObj,
    [ "IsVectorSpace and HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra",
      "IsVectorSpace and HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra" ],
    100,  # beat generic methods for two 'IsFLMLORWithOne'
    function( A, B )
    local P, basis, data, S;

    P:= Parent( A );
    if not IsIdenticalObj( P, Parent( B ) ) then
      TryNextMethod();
    fi;
    basis:= BasisVectors( CanonicalBasis( P ) );
    data:= Intersection2(
               GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( A ),
               GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( B ) );
    S:= SubspaceNC( P, basis{ data }, "basis" );
    SetGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( S, data );

    return S;
    end );


#############################################################################
##
#M  \+( <A>, <B> )
##
InstallOtherMethod( \+,
    IsIdenticalObj,
    [ "IsVectorSpace and HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra",
      "IsVectorSpace and HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra" ],
    100,  # beat generic methods for two rings, two left modules
    function( A, B )
    local P, basis, data, S;

    P:= Parent( A );
    if not IsIdenticalObj( P, Parent( B ) ) then
      TryNextMethod();
    fi;
    basis:= BasisVectors( CanonicalBasis( P ) );
    data:= Union(
               GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( A ),
               GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( B ) );
    S:= SubspaceNC( P, basis{ data }, "basis" );
    SetGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( S, data );

    return S;
    end );


#############################################################################
##
#M  ProductSpace( <A>, <B> )
##
InstallMethod( ProductSpace,
    IsIdenticalObj,
    [ "IsVectorSpace and HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra",
      "IsVectorSpace and HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra" ],
    100,  # beat generic methods for two algebras, two left modules
    function( A, B )
    local P, basis, data, S;

    P:= Parent( A );
    if not IsIdenticalObj( P, Parent( B ) ) then
      TryNextMethod();
    fi;
    basis:= BasisVectors( CanonicalBasis( P ) );
    data:= LoewyStructureInfoGAP( P );
    data:= SingerAlg.BasisOfProductSpace( data,
               GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( A ),
               GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( B ) );
    S:= SubspaceNC( P, basis{ data }, "basis" );
    SetGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( S, data );

    return S;
    end );


#############################################################################
##
#F  CoefficientsQadicReversed( <k>, <z>, <q>, <n> )
##
InstallGlobalFunction( CoefficientsQadicReversed, function( k, z, q, n )
    local v, i, r, rq, d;

    v:= [];

    if k = z then
      for i in [ 1 .. n ] do
        v[i]:= q-1;
      od;
    else
      r:= k;
      for i in [ 1 .. n ] do
        rq:= r * q;
        d:= QuoInt( rq, z );
        r:= rq - d * z;
        v[i]:= d;
      od;
    fi;

    return v;
    end );


#############################################################################
##
#F  SingerAlg.MultTable( <data> )
##
##  <#GAPDoc Label="SingerAlg.MultTable">
##  <ManSection>
##  <Func Name="SingerAlg.MultTable" Arg='data'/>
##
##  <Description>
##  Let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A[q,z]</M>,
##  and let <M>B = B(A)</M>.
##  <Ref Func="SingerAlg.MultTable"/> returns the
##  <M>(z+1) \times (z+1)</M> matrix that contains at the position
##  <M>(i,j)</M> the value <M>i+j-1</M> if the product
##  <M>B_i \cdot B_j</M> is nonzero
##  (hence equal to <M>B_{{i+j-1}}</M>, the <M>i+j-1</M>-th basis vector),
##  and <M>0</M> otherwise.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 2, 3, 7 );;
##  gap> Display( SingerAlg.MultTable( data ) );
##  [ [  1,  2,  3,  4,  5,  6,  7,  8 ],
##    [  2,  0,  4,  0,  6,  0,  8,  0 ],
##    [  3,  4,  0,  0,  7,  8,  0,  0 ],
##    [  4,  0,  0,  0,  8,  0,  0,  0 ],
##    [  5,  6,  7,  8,  0,  0,  0,  0 ],
##    [  6,  0,  8,  0,  0,  0,  0,  0 ],
##    [  7,  8,  0,  0,  0,  0,  0,  0 ],
##    [  8,  0,  0,  0,  0,  0,  0,  0 ] ]
##  ]]></Example>
##  <P/>
##  The multiplication table of a Singer algebra of dimension <M>N</M>
##  has the value <M>N</M> on the antidiagonal (<M>i+j = N+1</M>),
##  is zero below the antidiagonal,
##  and contains only <M>N-i</M> and zero on the <M>i</M> parallel above the
##  antidiagonal.
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.MultTable:= function( data )
    local monoms, n, nn, mat, q, qhalf, i, j;

    if not IsBound( data.multtable ) then
      monoms:= data.monomials;
      n:= Length( monoms ); # z+1
      nn:= QuoInt( n+1, 2 );
      mat:= NullMat( n, n );
      q:= data.parameters[1];
      if q mod 2 = 1 then
        qhalf:= QuoInt( q, 2 ) + 1;
      else
        qhalf:= QuoInt( q, 2 );
      fi;
      for i in [ 1 .. nn ] do 
        for j in [ 1 .. i-1 ] do 
          if ForAll( monoms[i] + monoms[j], x -> x < q ) then
            mat[i,j]:= i+j-1;
            mat[j,i]:= i+j-1;
          fi;
        od;
        if ForAll( monoms[i], x -> x < qhalf ) then
          mat[i,i]:= i+i-1;
        fi;
      od;
      for i in [ nn+1 .. n ] do 
        # The [i,j] entry can be nonzero only if i+j <= z+2 holds.
        for j in [ 1 .. n+1-i ] do 
          if ForAll( monoms[i] + monoms[j], x -> x < q ) then
            mat[i,j]:= i+j-1;
            mat[j,i]:= i+j-1;
          fi;
        od;
      od;
      data.multtable:= mat;
    fi;

    return data.multtable;
    end;


#############################################################################
##
#F  SingerAlg.BasisOfSum( <data>, <I>, <J> )
#F  SingerAlg.BasisOfIntersection( <data>, <I>, <J> )
##
##  <#GAPDoc Label="SingerAlg.BasisOfSum">
##  <ManSection>
##  <Heading>SingerAlg.BasisOfSum and SingerAlg.BasisOfIntersection</Heading>
##  <Func Name="SingerAlg.BasisOfSum" Arg='data, I, J'/>
##  <Func Name="SingerAlg.BasisOfIntersection" Arg='data, I, J'/>
##
##  <Description>
##  For two subsets <A>I</A>, <A>J</A> of <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  these functions just return the union and the intersection,
##  respectively, of <A>I</A> and <A>J</A>.
##  <P/>
##  <A>I</A> and <A>J</A> describe subsets of a basis, which generate the
##  spaces <M>U</M> and <M>V</M>, say, then the result describes the subset
##  of this basis that generates the sum and the intersection, respectively,
##  of <M>U</M> and <M>V</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> SingerAlg.BasisOfSum( data, [ 1, 2, 3 ], [ 2, 4, 6 ] );
##  [ 1, 2, 3, 4, 6 ]
##  gap> SingerAlg.BasisOfIntersection( data, [ 1, 2, 3 ], [ 2, 4, 6 ] );
##  [ 2 ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasisOfSum:= { data, I, J } -> Union( I, J );

SingerAlg.BasisOfIntersection:= { data, I, J } -> Intersection( I, J );


#############################################################################
##
#F  SingerAlg.BasisOfProductSpace( <data>, <I>, <J> )
##
##  <#GAPDoc Label="SingerAlg.BasisOfProductSpace">
##  <ManSection>
##  <Func Name="SingerAlg.BasisOfProductSpace" Arg='data, I, J'/>
##
##  <Description>
##  Let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  let <M>B = B(A)</M>,
##  and let <A>I</A>, <A>J</A> be subsets of
##  <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  describing subspaces <M>U</M>, <M>V</M> of <M>A</M> with bases
##  <M>( B_i; i \in I )</M> and <M>( B_i; i \in J )</M>, respectively.
##  <Ref Func="SingerAlg.BasisOfProductSpace"/> returns the subset <M>K</M>
##  of <M>\{ 1, 2, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in K )</M> is a basis of the product space <M>U \cdot V</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 2, 7 );;
##  gap> radser:= SingerAlg.BasesOfRadicalSeries( data );
##  [ [ 2 .. 8 ], [ 4, 6, 7, 8 ], [ 8 ] ]
##  gap> SingerAlg.BasisOfProductSpace( data, radser[1], radser[1] );
##  [ 4, 6, 7, 8 ]
##  gap> SingerAlg.BasisOfProductSpace( data, radser[1], radser[2] );
##  [ 8 ]
##  gap> SingerAlg.BasisOfProductSpace( data, radser[2], radser[2] );
##  [  ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasisOfProductSpace:= function( data, I, J )
    local mat, basis, i, j;

    mat:= SingerAlg.MultTable( data );
    basis:= [];

    for i in I do
      for j in J do
        if mat[i,j] <> 0 then
          AddSet( basis, i+j-1 );
        fi;
      od;
    od;

    return basis;
    end;


#############################################################################
##
#F  SingerAlg.BasisOfIdeal( <data>, <I> )
##
##  <#GAPDoc Label="SingerAlg.BasisOfIdeal">
##  <ManSection>
##  <Func Name="SingerAlg.BasisOfIdeal" Arg='data, I'/>
##
##  <Description>
##  Let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  let <M>B = B(A)</M>,
##  and let <A>I</A> be a subset of <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  describing a subspace <M>U</M> of <M>A</M> with basis
##  <M>( B_i; i \in I )</M>.
##  <Ref Func="SingerAlg.BasisOfIdeal"/> returns the subset <M>J</M>
##  of <M>\{ 1, 2, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in J )</M> is a basis of the ideal <M>U \cdot A</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 2, 7 );;
##  gap> SingerAlg.BasisOfIdeal( data, [ 4 ] );
##  [ 4, 8 ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasisOfIdeal:= { data, I } -> SingerAlg.BasisOfProductSpace( data,
              I, [ 1 .. Length( data.monomials ) ] );


#############################################################################
##
#F  SingerAlg.BasisOfAnnihilator( <data>, <I> )
##
##  <#GAPDoc Label="SingerAlg.BasisOfAnnihilator">
##  <ManSection>
##  <Func Name="SingerAlg.BasisOfAnnihilator" Arg='data, I'/>
##
##  <Description>
##  Let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  let <M>B = B(A)</M>,
##  and let <A>I</A> be a subset of <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  describing a subspace <M>U</M> of <M>A</M> with basis
##  <M>( B_i; i \in I )</M>.
##  <Ref Func="SingerAlg.BasisOfAnnihilator"/> returns the subset <M>J</M>
##  of <M>\{ 1, 2, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in J )</M> is a basis of the annihilator
##  <M>\{ x \in A; x \cdot U = 0 \}</M> of <M>U</M> in <M>A</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 2, 7 );;
##  gap> radser:= SingerAlg.BasesOfRadicalSeries( data );
##  [ [ 2 .. 8 ], [ 4, 6, 7, 8 ], [ 8 ] ]
##  gap> List( radser, I -> SingerAlg.BasisOfAnnihilator( data, I ) );
##  [ [ 8 ], [ 4, 6, 7, 8 ], [ 2, 3, 4, 5, 6, 7, 8 ] ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasisOfAnnihilator:= function( data, list )
    local mat;

    mat:= SingerAlg.MultTable( data );
    return Filtered( [ 1 .. Length( mat ) ],
                     i -> ForAll( list, j -> mat[i,j] = 0 ) );
    end;


#############################################################################
##
#F  SingerAlg.BasesOfRadicalSeries( <data> )
##
##  <#GAPDoc Label="SingerAlg.BasesOfRadicalSeries">
##  <ManSection>
##  <Func Name="SingerAlg.BasesOfRadicalSeries" Arg='data'/>
##
##  <Description>
##  Let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  and let <M>B = B(A)</M>.
##  <Ref Func="SingerAlg.BasesOfRadicalSeries"/> returns the list
##  <M>[ I_1, I_2, \ldots, I_l ]</M> of subsets of
##  <M>\{ 1, 2, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in I_j )</M> is a basis of the <M>j</M>-th power of the
##  Jacobson radical <M>J</M> of <M>A</M>,
##  and such that <M>J^l</M> is nonzero and <M>J^{{l+1}}</M> is zero.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 2, 7 );;
##  gap> radser:= SingerAlg.BasesOfRadicalSeries( data );
##  [ [ 2 .. 8 ], [ 4, 6, 7, 8 ], [ 8 ] ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasesOfRadicalSeries:= function( data )
    local lay, ll, jbylayer, J, i;

    lay:= data.layers;
    ll:= data.LL;
    jbylayer:= List( [ 1 .. ll-1 ], i -> Positions( lay, i ) );
    J:= [ jbylayer[ ll-1 ] ];
    for i in [ 2 .. ll-1 ] do
      J[i]:= Union( J[ i-1 ], jbylayer[ ll - i ] );
    od;

    return Reversed( J );
    end;


#############################################################################
##
#F  SingerAlg.BasesOfSocleSeries( <data> )
##
##  <#GAPDoc Label="SingerAlg.BasesOfSocleSeries">
##  <ManSection>
##  <Func Name="SingerAlg.BasesOfSocleSeries" Arg='data'/>
##
##  <Description>
##  Let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  and let <M>B = B(A)</M>.
##  <Ref Func="SingerAlg.BasesOfSocleSeries"/> returns the list 
##  <M>[ I_1, I_2, \ldots, I_l ]</M> of subsets of 
##  <M>\{ 1, 2, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in I_j )</M> is a basis of <M>S_j</M>,
##  where <M>S_1</M> is the socle of <M>A</M>,
##  <M>S_{{j+1}}/S_j</M> is the socle of <A>A</A><M>/S_j</M>,
##  and <A>A</A><M>/S_l</M> is nonzero and its own socle.
##  <P/>
##  <Example><![CDATA[
##  gap> socser:= SingerAlg.BasesOfSocleSeries( data );
##  [ [ 8 ], [ 4, 6, 7, 8 ], [ 2 .. 8 ] ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasesOfSocleSeries:= function( data )
    local lay, ll, sbylayer, S, i;

    lay:= Reversed( data.layers );
    ll:= data.LL;
    sbylayer:= List( [ 0 .. ll-2 ], i -> Positions( lay, i ) );
    S:= [ sbylayer[ 1 ] ];
    for i in [ 2 .. ll-1 ] do
      S[i]:= Union( S[ i-1 ], sbylayer[i] );
    od;

    return S;
    end;


#############################################################################
##
#F  SingerAlg.BasisOfPowers( <data>, <I>, <p>, <m> )
##
##  <#GAPDoc Label="SingerAlg.BasisOfPowers">
##  <ManSection>
##  <Func Name="SingerAlg.BasisOfPowers" Arg='data, I, p, m'/>
##
##  <Description>
##  Let <A>p</A> be a prime integer,
##  let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  let <M>B = B(A_p)</M>,
##  let <A>I</A> be a subset of <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  describing a subspace <M>U</M> of <M>A_p</M> with basis
##  <M>( B_i; i \in I )</M>,
##  and let <A>m</A> be a positive integer.
##  <Ref Func="SingerAlg.BasisOfPowers"/> returns the subset <M>J</M>
##  of <M>\{ 1, 2, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in J )</M> is a basis of the subspace
##  <M>\{ x^{{p^m}}; x \in U \}</M> of <M>A_p</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 3, 8 );;
##  gap> SingerAlg.BasisOfPowers( data, [ 1 .. 9 ], 2, 1 );
##  [ 1, 3, 7, 9 ]
##  gap> SingerAlg.BasisOfPowers( data, [ 1 .. 9 ], 2, 2 );
##  [ 1 ]
##  gap> SingerAlg.BasisOfPowers( data, [ 1 .. 9 ], 3, 1 );
##  [ 1 ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasisOfPowers:= function( data, I, p, m )
    local result, q, pm, quo, k;

    result:= [];
    q:= data.parameters[1];
    pm:= p^m;
    quo:= QuoInt( q-1, pm );
    for k in I do
      if Maximum( data.monomials[k] ) <= quo then
        Add( result, pm * ( k-1 ) + 1 );
      fi;
    od;

    return result;
    end;


#############################################################################
##
#F  SingerAlg.BasisOfPMRoots( <data>, <I>, <p>, <m> )
##
##  <#GAPDoc Label="SingerAlg.BasisOfPMRoots">
##  <ManSection>
##  <Func Name="SingerAlg.BasisOfPMRoots" Arg='data, I, p, m'/>
##
##  <Description>
##  Let <A>p</A> be a prime integer,
##  let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  let <M>B = B(A_p)</M>,
##  let <A>I</A> be a subset of <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  describing a subspace <M>U</M> of <M>A_p</M> with basis
##  <M>( B_i; i \in I )</M>,
##  and let <A>m</A> be a positive integer.
##  (See Section <Ref Sect="sect:Singer algebras"/> for the definition of
##  <M>A_p</M>.)
##  <P/>
##  <Ref Func="SingerAlg.BasisOfPMRoots"/> returns the subset <M>J</M>
##  of <M>\{ 1, 2, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in J )</M> is a basis of the subspace
##  <M>\{ x \in A_p; x^{{p^m}} \in U \}</M> of <M>A_p</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 3, 8 );;
##  gap> SingerAlg.BasisOfPMRoots( data, [], 2, 1 );
##  [ 3, 6, 7, 8, 9 ]
##  gap> SingerAlg.BasisOfPMRoots( data, [], 2, 2 );
##  [ 2, 3, 4, 5, 6, 7, 8, 9 ]
##  gap> SingerAlg.BasisOfPMRoots( data, [ 3 ], 2, 1 );
##  [ 2, 3, 6, 7, 8, 9 ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasisOfPMRoots:= function( data, I, p, m )
    local q, pm, quo;

    q:= data.parameters[1];
    pm:= p^m;
    quo:= QuoInt( q-1, pm );

    # The first condition means that B_k^{p^m} is zero
    # and hence inside the given subspace.
    # For the second condition, we may assume that B_k^{p^m}
    # is nonzero and hence equal to B_{p^m (k-1)+1}.
    return Filtered( [ 1 .. Length( data.monomials ) ],
                     k ->    Maximum( data.monomials[k] ) > quo
                          or pm * (k-1) + 1 in I );
    end;


#############################################################################
##
#F  SingerAlg.BasisOfPC( <data>, <I>, <J> )
##
##  <#GAPDoc Label="SingerAlg.BasisOfPC">
##  <ManSection>
##  <Func Name="SingerAlg.BasisOfPC" Arg='data, I, J'/>
##
##  <Description>
##  Let <A>data</A> be the record returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  let <M>B = B(A)</M>,
##  let <A>I</A> and <M>J</M> be subsets of <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  describing subspaces <M>U</M> and <M>V</M> of <M>A</M> with bases
##  <M>( B_i; i \in I )</M> and <M>( B_i; i \in J )</M>,
##  respectively.
##  <Ref Func="SingerAlg.BasisOfPC"/> returns the subset <M>K</M>
##  of <M>\{ 2, 3, \ldots, z+1 \}</M> such that
##  <M>( B_i; i \in K )</M> is a basis of the subspace
##  <M>\{ x \in J(A); x \cdot U \subseteq V \}</M> of <M>J(A)</M>.
##  <P/>
##  (The perhaps strange name <Q>BasisOfPC</Q> was chosen because the
##  result contains the indices of those basis vectors such that the
##  <E>P</E>roduct with the space <M>U</M> is <E>C</E>ontained in the
##  space <M>V</M>.)
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 23, 585 );;
##  gap> soc:= SingerAlg.BasesOfSocleSeries( data );;
##  gap> rad:= SingerAlg.BasesOfRadicalSeries( data );;
##  gap> I1:= SingerAlg.BasisOfPC( data, soc[3], rad[3] );;
##  gap> Length( I1 );
##  581
##  gap> data:= LoewyStructureInfoGAP( 212, 585 );;
##  gap> soc:= SingerAlg.BasesOfSocleSeries( data );;
##  gap> rad:= SingerAlg.BasesOfRadicalSeries( data );;
##  gap> I2:= SingerAlg.BasisOfPC( data, soc[3], rad[3] );;
##  gap> Length( I2 );
##  545
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.BasisOfPC:= { data, I, J } -> Filtered(
    # Note that we are interested in subspaces of the radical.
    [ 2 .. Length( data.monomials ) ],
    k -> IsSubset( J, SingerAlg.BasisOfProductSpace( data, [ k ], I ) ) );


#############################################################################
##
#E

