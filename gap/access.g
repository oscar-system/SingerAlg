#############################################################################
##
#W  access.g             GAP 4 package SingerAlg                Thomas Breuer
##
##  This file contains GAP functions related to the database of
##  Singer algebras <M>A[q,z]</M> with <M>z \leq &MAXZ;</M>.
##


#############################################################################
##
##  <#GAPDoc Label="SingerAlgebraDatabase">
##  <Section>
##  <Heading>Overview</Heading>
##
##  Fix a positive integer <M>z</M>.
##  In order to describe all Singer algebras <M>A[q,z]</M>,
##  it is sufficient to consider one representative <M>q</M> for each cyclic
##  subgroup of the group of prime residues modulo <M>z</M>,
##  see Section <Ref Sect="sect:Singer algebras"/>.
##  The database of Singer algebras with <M>1 \leq z \leq &MAXZ;</M>
##  is built according to this observation.
##  That is, there is one entry for each such parameter pair <M>(q,z)</M>,
##  where we choose the smallest <M>q</M> from the generators of the
##  subgroup it generates,
##  except that <M>q = z+1</M> is chosen instead of <M>q = 1</M>.
##  <P/>
##  This implies that the number of data records for given <M>z</M>
##  is exactly
##  <M>\sum_q 1/</M><C>Phi( </C><M>&ord;_z(q)</M><C> )</C>.
##
##  <Log>
##  gap> ForAll( [ 1 .. &MAXZ; ],
##  >            z -> Length( AllSingerAlgebraInfos( "z", z ) ) =
##  >                 Sum( PrimeResidues( z ),
##  >                      q -> 1 / Phi( OrderMod( q, z ) ) ) );
##  true
##  </Log>
##
##  <!-- This test takes about 277 seconds,
##       it is contained in 'tst/hard.tst'. -->
##
##  Note that two algebras <M>A[q,z]</M>, <M>A[q',z]</M> for different
##  such representatives <M>q</M>, <M>q'</M> can be isomorphic,
##  and in fact this happens in many cases in a <Q>natural</Q> way
##  (see Section <Ref Subsect="subsect:canoniso"/>).
##  As soon as new theoretical criteria become known that admit a reduction
##  of the set of parameters to describe all Singer algebras of a given
##  dimension, the setup of the database may be changed.
##  <P/>
##  The database stores the following information for the algebra
##  <M>A[q,z]</M>.
##  <P/>
##  <List>
##  <Item>
##    <M>n = &ord;_z(q)</M>,
##    the multiplicative order of <M>q</M> modulo <M>z</M>
##    (<C>OrderMod( </C><M>q</M><C>, </C><M>z</M><C> )</C>, see
##    <Ref Func="OrderModExt"/>),
##  </Item>
##  <Item>
##    <M>m = m(q,e)</M>,
##    the minimal number of powers of <M>q</M> whose sum is divisible
##    by <M>e = (q^n-1)/z</M>,
##  </Item>
##  <Item>
##    <M>l</M>, the Loewy length of <M>A[q,z]</M>,
##  </Item>
##  <Item>
##    the Loewy vector <M>(v_1, v_2, \ldots, v_l)</M> of <M>A[q,z]</M>,
##    where <M>v_i = \dim( J^{{i-1}} / J^i )</M>,
##    and <M>J = J(A[q,z])</M> is the Jacobson radical of <M>A[q,z]</M>;
##    we encode <M>v</M> by an abbreviated vector <M>v'</M>
##    (see <Ref Func="LoewyVectorAbbreviated"/>)
##    where subsequent equal entries <M>i</M>
##    with multiplicity <M>j \geq 1</M> are abbreviated as <M>[ i, j ]</M>;
##    thus <M>v' = [ [ 1, j ] ]</M> stands for
##    <M>v = [ 1, 1, \ldots, 1 ]</M> of length <M>j</M>;
##  </Item>
##  <Item>
##    <Q>decomposition information</Q> <M>d</M>, if available, as follows:
##    Set <M>q' = q \bmod m</M>; a decomposition
##    <M>(x_1 \cdots x_n)^{{q'-1}} = \prod_{{i=1}}^N
##    (x_1^{{a_{{i,1}}}} x_2^{{a_{{i,2}}}} \cdots x_n^{{a_{{i,n}}}})</M>
##    of length <M>N = l-1-n(q-q')/m</M> is encoded by
##    <M>d = [ k_1, k_2, \ldots, k_N ]</M>,
##    where <M>e k_i = \sum_{{j=1}}^n a_{{i,j}} q^{{j-1}}</M> holds;
##    if no such decomposition is known then <M>d = 0</M> is stored,
##  </Item>
##  <Item>
##    the flag <M>dec</M> is <M>0</M> if <M>(x_1 \cdots x_n)^{{q'-1}}</M>
##    can be written as a product of monomials as above, each of degree
##    <M>m</M> except at most one monomial of larger degree,
##    and <M>1</M> otherwise,
##  </Item>
##  <Item>
##    <M>delta = \lfloor n (q-1)/m \rfloor + 1 - l</M> is the difference
##    between the upper bound from <Cite Key="BHHK1" Where="Theorem 7.1"/>
##    and the Loewy length <M>l</M>,
##  </Item>
##  <Item>
##    the <Ref Attr="IdSingerAlgebra" Label="for a Singer algebra"/> value
##    of <M>A[q,z]</M>.
##  </Item>
##  </List>
##  </Section>
##  <#/GAPDoc>
##


#############################################################################
##
#F  SingerAlg.ContentsOfDataFile( <filename> )
##
##  <#GAPDoc Label="SingerAlg.ContentsOfDataFile">
##  <ManSection>
##  <Attr Name="SingerAlg.ContentsOfDataFile" Arg='filename'/>
##
##  <Returns>
##  the &GAP; object stored in the given file.
##  </Returns>
##  <Description>
##  Let <A>filename</A> be a string that denotes the name of a file in the
##  <F>data</F> subdirectory of the &SingerAlg; package directory.
##  <Ref Func="SingerAlg.ContentsOfDataFile"/> evaluates the contents of
##  this file (assuming that it is &GAP; readable) and returns the result.
##  <P/>
##  <Example><![CDATA[
##  gap> val:= SingerAlg.ContentsOfDataFile( "mqe.json" );;
##  gap> val[1][2];
##  "This file contains the sorted list of values '[ e, q, m(q,e) ]', "
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.ContentsOfDataFile:= function( filename )
    Info( InfoSingerAlg, 1, "loading data file '", filename, "'" );
    return EvalString( StringFile( Filename( DirectoriesPackageLibrary(
               "SingerAlg", "data" ), filename ) ) );
    end;


SingerAlg.MaxZ:= 10000;

SingerAlg.SmallestGeneratorPrimeResidues:= function( q, z )
    local ord, min, i, cand;

    ord:= OrderMod( q, z );
    if ord = 0 then
      # 'z' and 'q' are not coprime.
      return fail;
    elif ord = 1 then
      # special case: choose 'z+1' not '1' as representative
      return z+1;
    fi;
    min:= q;
    for i in PrimeResidues( ord ) do
      cand:= PowerMod( q, i, z );
      if cand < min then
        min:= cand;
      fi;
    od;
    return min;
    end;

SingerAlg.LoewyVectorsDataByZ:= [];

BindGlobal( "SingerAlgebraData", function( z, q... )
    local i, istr, eval, range, j, l, r, idlist;

    if not IsPosInt( z ) or z > SingerAlg.MaxZ then
      Error( "<z> must be an integer in [ 1 .. ", SingerAlg.MaxZ, " ]" );
    elif not IsBound( SingerAlg.LoewyVectorsDataByZ[z] ) then
      # The data files contain algebras for 'z' in the intervals
      # [ 1 .. 500 ], [ 501 .. 1000 ], [ 1001 .. 1500 ], ...
      i:= QuoInt( z-1, 500 ) + 1;
      if i < 10 then
        istr:= Concatenation( "0", String( i ) );
      else
        istr:= String( i );
      fi;

      if not IsBound( SingerAlg.IdData ) then
        # Initialize the isomorphism type info.
        SingerAlg.IdData:= SingerAlg.ContentsOfDataFile( "id.json" );
      fi;

      eval:= SingerAlg.ContentsOfDataFile(
                 Concatenation( "lvdata", istr, ".json" ) );
      range:= [ eval[2][1] .. eval[2][2] ];
      Assert( 1, range = [ (i-1)*500+1 .. i*500 ],
              "wrong range in data file" );
      for j in range do
        SingerAlg.LoewyVectorsDataByZ[j]:= [];
      od;
      for l in eval[3] do
        r:= rec( z:= l[1], q:= l[2], n:= l[3],
                 e:= (l[2]^l[3]-1)/l[1], m:= l[4], LL:= l[5],
                 vprime:= l[6], d:= l[7],
                 dec:= l[8], diff:= l[9] );
        if l[7] = 0 then
          r.d:= fail;
        fi;
        # Store the info about the isomorphism type.
        idlist:= First( SingerAlg.IdData[2][ r.z ][2], l -> r.q in l );
        if idlist = fail then
          r.isom:= [ r.z, r.q ];
        elif idlist[1] = 0 then
          r.isom:= fail;
        else
          r.isom:= [ r.z, idlist[1] ];
        fi;
        Add( SingerAlg.LoewyVectorsDataByZ[ l[1] ], r );
      od;
      for j in range do
        MakeImmutable( SingerAlg.LoewyVectorsDataByZ[j] );
      od;
    fi;

    if Length( q ) = 0 then
      # Return the list of all records for 'z'.
      return SingerAlg.LoewyVectorsDataByZ[z];
    elif Length( q ) = 1 then
      # Return the record for 'z' and 'q'.
      q:= SingerAlg.SmallestGeneratorPrimeResidues( q[1], z );
      if q = fail then
        Error( "<z> and <q> must be coprime" );
      fi;
      return First( SingerAlg.LoewyVectorsDataByZ[z], r -> r.q = q );
    else
      Error( "usage: SingerAlgebraData( <z>[, <q>] )" );
    fi;
    end );


#############################################################################
##
#F  SomeSingerAlgebraInfos( <condsvals>, <all> )
##
##  If <all> is 'true', returns the list of all data records that satisfy
##  the conditions in the list <condsvals>.
##  If <all> is 'false', returns either one such data record or 'fail'.
##
BindGlobal( "SomeSingerAlgebraInfos", function( condsvals, all )
    local maxz, crit1, crit2, zs, i, indiv, result, z, r;

    if not IsEvenInt( Length( condsvals ) ) then
      Error( "<condsvals> must have an even number of entries" );
    fi;

    maxz:= SingerAlg.MaxZ;

    crit1:= [];
    crit2:= [];
    zs:= [ 1 .. maxz ];

    for i in [ 1, 3 .. Length( condsvals ) - 1 ] do
      if condsvals[i] = "z" then
        zs:= condsvals[ i+1 ];
      elif condsvals[i] = Dimension then
        zs:= condsvals[ i+1 ]-1;
      elif condsvals[i] = "v" or condsvals[i] = LoewyVector then
        # Rewrite 'condsvals[ i+1 ]' to vprime format.
        Add( crit1, [ "vprime", LoewyVectorAbbreviated( condsvals[ i+1 ] ) ] );
      elif condsvals[i] = "LL" or condsvals[i] = LoewyLength then
        Add( crit1, [ "LL", condsvals[ i+1 ] ] );
      elif condsvals[i] in [ "n", "q", "m", "e", "vprime", "diff", "isom" ] then
        Add( crit1, [ condsvals[i], condsvals[ i+1 ] ] );
      elif IsFunction( condsvals[i] ) then
        Add( crit2, [ condsvals[i], condsvals[ i+1 ] ] );
      else
        Error( "no support for criterion about '", condsvals[i], "'" );
      fi;
    od;

    if IsInt( zs ) then
      if IsPosInt( zs ) and zs <= maxz then
        zs:= [ zs ];
      elif all then
        return [];
      else
        return fail;
      fi;
    elif IsFunction( zs ) then
      zs:= Filtered( [ 1 .. maxz ], z -> zs( z ) = true );
    elif IsList( zs ) then
      zs:= Intersection( zs, [ 1 .. maxz ] );
    else
      Error( "condition for \"z\" must be integer, function, or list" );
    fi;

    indiv:= function( fun, val, r )
      local target;

      target:= fun( r );
      return ( IsFunction( val ) and val( target ) = true ) or
             ( IsList( val ) and target in val ) or
             ( target = val );
    end;

    result:= [];
    for z in zs do
      for r in SingerAlgebraData( z ) do
        if ForAll( crit1,
                   pair -> ( IsFunction( pair[2] ) and
                             pair[2]( r.( pair[1] ) ) = true ) or
                           ( IsList( pair[2] ) and r.( pair[1] ) in pair[2] ) or
                           ( r.( pair[1] ) = pair[2] ) ) and
           ForAll( crit2, pair -> indiv( pair[1], pair[2], r ) ) then
          if all = true then
            Add( result, r );
          else
            return r;
          fi;
        fi;
      od;
    od;

    if all = true then
      return result;
    else
      return fail;
    fi;
    end );


#############################################################################
##
#F  OneSingerAlgebraInfo( <cond1>, <val1>, <cond2>, <val2>, ... )
#F  AllSingerAlgebraInfos( <cond1>, <val1>, <cond2>, <val2>, ... )
##
##  <#GAPDoc Label="OneSingerAlgebraInfo">
##  <ManSection>
##  <Heading>OneSingerAlgebraInfo and AllSingerAlgebraInfos</Heading>
##  <Func Name="OneSingerAlgebraInfo" Arg="cond1, val1, cond2, val2, ..."/>
##  <Func Name="AllSingerAlgebraInfos" Arg="cond1, val1, cond2, val2, ..."/>
##
##  <Description>
##  Let <A>cond1</A>, <A>cond2</A>, <M>\ldots</M> be strings that describe
##  precomputed properties of Singer algebras from the database,
##  that is, they occur in the set
##  <M>\{</M> <C>"z"</C>, <C>"q"</C>, <C>"n"</C>, <C>"m"</C>, <C>"e"</C>,
##  <C>"vprime"</C>, <C>"diff"</C> <M>\}</M> (see above).
##  The two functions compute those database entries such that the value for
##  <A>cond1</A> matches <A>val1</A>,
##  the value for <A>cond2</A> matches <A>val2</A>, etc.,
##  where <Q>matches</Q> means one of the following.
##  <P/>
##  <List>
##  <Item>
##    <A>val</A> is equal to the <A>cond</A> component of the entry,
##  </Item>
##  <Item>
##    <A>val</A> is a list in which the <A>cond</A> component of the entry
##    occurs, or
##  </Item>
##  <Item>
##    <A>val</A> is a unary function that returns <K>true</K> for the
##    <A>cond</A> component of the entry.
##  </Item>
##  </List>
##  <P/>
##  It is also possible to enter &GAP; functions as some of the <A>cond</A>
##  arguments.
##  Each such function must take a record as is returned by
##  <Ref Func="OneSingerAlgebraInfo"/>,
##  and the value returned by the function gets compared with the
##  corresponding <A>val</A> argument in the same way as described above.
##  <P/>
##  <Ref Func="OneSingerAlgebraInfo"/> returns the first matching entry
##  if there is one, and <K>fail</K> otherwise.
##  <Ref Func="AllSingerAlgebraInfos"/> returns the set of all matching
##  entries.
##  <P/>
##  <Example>
##  gap> OneSingerAlgebraInfo( "z", 8 );                                  
##  rec( LL := 5, d := [  ], dec := 0, diff := 0, e := 1, 
##    isom := [ 8, 3 ], m := 1, n := 2, q := 3, 
##    vprime := [ 1, 2, 3, 2, 1 ], z := 8 )
##  gap> AllSingerAlgebraInfos( "diff", 1, "e", IsPrimeInt );        
##  [ rec( LL := 3, d := fail, dec := 1, diff := 1, 
##        e := 380808546861411923, isom := [ 862, 3 ], m := 26, n := 43, 
##        q := 3, vprime := [ 1, 861, 1 ], z := 862 ) ]
##  gap> OneSingerAlgebraInfo( "z", 8, r -> r.m, 2 );
##  rec( LL := 5, d := [  ], dec := 0, diff := 0, e := 3, 
##    isom := [ 8, 5 ], m := 2, n := 2, q := 5, 
##    vprime := [ 1, [ 3, 2 ], [ 1, 2 ] ], z := 8 )
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
BindGlobal( "OneSingerAlgebraInfo", function( arg )
    return SomeSingerAlgebraInfos( arg, false );
    end );

BindGlobal( "AllSingerAlgebraInfos", function( arg )
    return SomeSingerAlgebraInfos( arg, true );
    end );


#############################################################################
##
#O  IdSingerAlgebra( <q>, <z> )
#A  IdSingerAlgebra( <A> )
##
##  <#GAPDoc Label="IdSingerAlgebra">
##  <ManSection>
##  <Heading>IdSingerAlgebra</Heading>
##  <Oper Name="IdSingerAlgebra" Arg="q, z" Label="for parameters"/>
##  <Attr Name="IdSingerAlgebra" Arg="A" Label="for a Singer algebra"/>
##
##  <Returns>
##  a pair of positive integers, or <K>fail</K>.
##  </Returns>
##  <Description>
##  For positive integers <A>q</A> and <A>z</A>,
##  <Ref Attr="IdSingerAlgebra" Label="for parameters"/> returns
##  either <K>fail</K> (if the pair <M>[ <A>q</A>, <A>z</A> ]</M> belongs to
##  the set of those parameters for which the distribution to isomorphism
##  types is not yet known)
##  or the list <M>[ <A>z</A>, q' ]</M> such that <M>q'</M> is minimal with
##  the property that the Singer algebra <M>A[q',<A>z</A>]</M> is isomorphic
##  with <M>A[<A>q</A>,<A>z</A>]</M>.
##  <P/>
##  For a Singer algebra <A>A</A> <M>= A[q,z]</M> of dimension
##  <M>z + 1 \leq 10001</M> and with known
##  <Ref Attr="ParametersOfSingerAlgebra"/> value,
##  <Ref Attr="IdSingerAlgebra" Label="for a Singer algebra"/> returns the
##  value for the arguments <M>q</M> and <M>z</M>.
##  <P/>
##  <Example>
##  gap> List( [ 2 .. 8 ], q -> IdSingerAlgebra( q, 7 ) );
##  [ [ 7, 2 ], [ 7, 3 ], [ 7, 2 ], [ 7, 3 ], [ 7, 3 ], fail, [ 7, 8 ] ]
##  gap> IdSingerAlgebra( 68, 171 );
##  fail
##  gap> IdSingerAlgebra( SingerAlgebra( 10, 11 ) );
##  [ 11, 2 ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "IdSingerAlgebra", IsSingerAlgebra );

DeclareOperation( "IdSingerAlgebra", [ IsPosInt, IsPosInt ] );

InstallMethod( IdSingerAlgebra,
    [ "IsSingerAlgebra and HasParametersOfSingerAlgebra" ],
    A -> CallFuncList( IdSingerAlgebra,
                       ParametersOfSingerAlgebra( A ){ [ 1, 3 ] } ) );

InstallMethod( IdSingerAlgebra,
    [ "IsPosInt", "IsPosInt" ],
    function( q, z )
    local l;

    if z > SingerAlg.MaxZ then
      return fail;
    fi;

    # Make sure that 'q' is a smallest representative.
    q:= SingerAlg.SmallestGeneratorPrimeResidues( q, z );

    if q = fail then
      return fail;
    elif not IsBound( SingerAlg.IdData ) then
      # Initialize the isomorphism type info.
      SingerAlg.IdData:= SingerAlg.ContentsOfDataFile( "id.json" );
    fi;

    for l in SingerAlg.IdData[2][z][2] do
      if q in l then
        q:= l[1];
        if q = 0 then
          # The isomorphism type is not yet decided.
          return fail;
        else
          return [ z, q ];
        fi;
      fi;
    od;

    # 'q' stands for itself.
    return [ z, q ];
    end );


#############################################################################
##
#F  SingerAlg.ComputedIdInfoForSingerAlgebras()
##
##  This function returns a string to be used as the second entry in
##  the file 'data/id.json'.
##  It assumes that the distribution of parameter pairs to isomorphism
##  classes as described in the package manual is stored in the list
##  'KnownDistribution';
##  for that, one can run 'Test' on the package file 'tst/docxpl.tst'.
##
SingerAlg.ComputedIdInfoForSingerAlgebras:= function()
    local dist, list, z, entry;

    if not IsBound( KnownDistribution ) then
      Error( "It is assumed that the (partial) classification ",
             "of isomorphism classes from 'docxpl.tst' gets computed ",
             "before this function gets called." );
    fi;

    dist:= ValueGlobal( "KnownDistribution" );
    list:= [];
    for z in [ 1 .. Length( dist ) ] do
      list[z]:= [ z, [] ];
      for entry in dist[z] do
        if Length( entry ) <> 1 then
          # The isomorphism type is not yet determined.
          AddSet( list[z][2],
                  Concatenation( [ 0 ],
                                 SortedList( Concatenation( entry ) ) ) );
        elif Length( entry[1] ) <> 1 then
          # More than one 'q' belongs to the isomorphism class.
          AddSet( list[z][2], SortedList( entry[1] ) );
        fi;
      od;
    od;

    return JoinStringsWithSeparator(
               List( list, l -> ReplacedString( String( l ), " ", "" ) ),
               ",\n" );
    end;


#############################################################################
##
#F  SingerAlg.ComputedOpenCases()
##
##  This function returns a string to be used as the second entry in
##  the file 'data/opencases.json'.
##  It assumes that the distribution of parameter pairs to isomorphism
##  classes as described in the package manual is stored in the list
##  'KnownDistribution';
##  for that, one can run 'Test' on the package file 'tst/docxpl.tst'.
##
SingerAlg.ComputedOpenCases:= function()
    local dist, list, z, filt;

    if not IsBound( KnownDistribution ) then
      Error( "It is assumed that the (partial) classification ",
             "of isomorphism classes from 'docxpl.tst' gets computed ",
             "before this function gets called." );
    fi;

    dist:= ValueGlobal( "KnownDistribution" );
    list:= [];
    for z in [ 1 .. Length( dist ) ] do
      filt:= Filtered( dist[z], l -> Length( l ) > 1 );
      if Length( filt ) > 0 then
        Add( list, [ z, filt ] );
      fi;
    od;

    return JoinStringsWithSeparator(
               List( list, l -> ReplacedString( String( l ), " ", "" ) ),
               ",\n" );
    end;


#############################################################################
##
#E

