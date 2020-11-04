#############################################################################
##
#W  permiso.g            GAP 4 package SingerAlg                Thomas Breuer
##
##  This file contains code for deciding whether two Singer algebras are
##  permutation isomorphic in the sense that mapping the permuted canonical
##  basis of the first to the canonical basis of the second defines an
##  algebra isomorphism.
##


#############################################################################
##
##  Define some utilities.
##
SingerAlg.RefinePartitions:= function( pi1, set1, pi2, set2 )
    local result1, result2, i, new1, new2;

    result1:= [];
    result2:= [];
    for i in [ 1 .. Length( pi1 ) ] do
      new1:= [ Intersection( pi1[i], set1 ), Difference( pi1[i], set1 ) ];
      new2:= [ Intersection( pi2[i], set2 ), Difference( pi2[i], set2 ) ];
      if List( new1, Length ) <> List( new2, Length ) then
        return false;
      fi;
      Append( result1, Filtered( new1, x -> x <> [] ) );
      Append( result2, Filtered( new2, x -> x <> [] ) );
    od;
    for i in [ 1 .. Length( result1 ) ] do
      pi1[i]:= result1[i];
      pi2[i]:= result2[i];
    od;
    return true;
end;


SingerAlg.OverflowCount:= function( data )
    local mat, list;

    if not IsBound( data.overflowCount ) then
      mat:= SingerAlg.MultTable( data );
      list:= List( mat, row -> Number( row, x -> x <> 0 ) );
      data.overflowCount:= list;
    fi;

    return data.overflowCount;
end;


##  Compute all relevant p^m-th powers at once.
##
SingerAlg.BasesOfPPowers:= function( data )
    local result, I, p, m, res, powers;

    result:= [];
    I:= [ 2 .. Length( data.monomials ) ];
    p:= 2;
    while true do
      m:= 1;
      res:= [];
      while true do
        powers:= SingerAlg.BasisOfPowers( data, I, p, m );
        if IsEmpty( powers ) then
          break;
        fi;
        Add( res, powers );
        m:= m + 1;
      od;
      if IsEmpty( res ) then
        break;
      fi;
      Add( result, res );
      p:= NextPrimeInt( p );
    od;
      
    return result;
end;


SingerAlg.CommonPartition:= function( data1, data2 )
    local z, dim, lens1, lens2, lensset, part1, i, part2, jbases1, jbases2,
          jlayers1, jlayers2, freemult1, freemult2, bases1, bases2,
          psi1, psi2;

    z:= data1.parameters[3];
    dim:= z + 1;;
    if z <> data2.parameters[3] then
      return "different dimensions";
    fi;

    # numbers of non-overflows per row
    lens1:= SingerAlg.OverflowCount( data1 );
    lens2:= SingerAlg.OverflowCount( data2 );
    if Collected( lens1 ) <> Collected( lens2 ) then
      # cannot be isomorphic
      return "different distributions of products";
    fi;;
    lensset:= Set( lens1 );
    part1:= List( lensset, x -> [] );
    for i in [ 1 .. Length( lens1 ) ] do
      Add( part1[ Position( lensset, lens1[i] ) ], i );
    od;
    part2:= List( lensset, x -> [] );
    for i in [ 1 .. Length( lens2 ) ] do
      Add( part2[ Position( lensset, lens2[i] ) ], i );
    od;
    if List( part1, Length ) <> List( part2, Length ) then
      return "different initial partitions";
    fi;

    # remove b_0 and b_z
    part1:= Filtered( part1, x -> x <> [ 1 ] and x <> [ dim ] );;
    part2:= Filtered( part2, x -> x <> [ 1 ] and x <> [ dim ] );;

    # refine by the Loewy layers
    jbases1:= SingerAlg.BasesOfRadicalSeries( data1 );
    jbases2:= SingerAlg.BasesOfRadicalSeries( data2 );
    jlayers1:= List( [ 1 .. Length( jbases1 ) - 1 ],
                     i -> Difference( jbases1[i], jbases1[i+1] ) );
    jlayers2:= List( [ 1 .. Length( jbases2 ) - 1 ],
                     i -> Difference( jbases2[i], jbases2[i+1] ) );
    if List( jlayers1, Length ) <> List( jlayers2, Length ) then
      return "different Loewy layers";
    fi;

    # the basis vectors b_i with multiplication either zero or b_z
    # such that both b_{z-i} do not themselves occur as products
    freemult1:= Intersection( part1[1], jlayers1[1] );
    freemult1:= Intersection( freemult1, List( freemult1, x -> z+2-x ) );
    freemult2:= Intersection( part2[1], jlayers2[1] );
    freemult2:= Intersection( freemult2, List( freemult2, x -> z+2-x ) );

    for i in [ 1 .. Length( jlayers1 ) ] do
      if not SingerAlg.RefinePartitions( part1, jlayers1[i],
                                         part2, jlayers2[i] ) then
        return "different Loewy refinements";
      fi;
    od;

    # refine by the socle layers
    bases1:= SingerAlg.BasesOfSocleSeries( data1 );
    bases2:= SingerAlg.BasesOfSocleSeries( data2 );
    psi1:= List( [ 1 .. Length( bases1 ) - 1 ],
                 i -> Difference( bases1[i+1], bases1[i] ) );
    psi2:= List( [ 1 .. Length( bases2 ) - 1 ],
                 i -> Difference( bases2[i+1], bases2[i] ) );
    if List( psi1, Length ) <> List( psi2, Length ) then
      return "different socle layers";
    fi;
    for i in [ 1 .. Length( psi1 ) ] do
      if not SingerAlg.RefinePartitions( part1, psi1[i], part2, psi2[i] ) then
        return "different socle refinements";
      fi;
    od;

    # refine by p^m-th powers
    bases1:= SingerAlg.BasesOfPPowers( data1 );
    bases2:= SingerAlg.BasesOfPPowers( data2 );
    if List( bases1, Length ) <> List( bases2, Length ) then
      return "different p-power structures";
    fi;
    psi1:= Concatenation( bases1 );
    psi2:= Concatenation( bases2 );
    if List( psi1, Length ) <> List( psi2, Length ) then
      return "different p-power structures";
    fi;
    for i in [ 1 .. Length( psi1 ) ] do
      if not SingerAlg.RefinePartitions( part1, psi1[i], part2, psi2[i] ) then
        return "different p-power refinements";
      fi;
    od;

    return rec( part1:= part1, part2:= part2,
                freemult1:= freemult1, freemult2:= freemult2 );
end;


#############################################################################
##
#F  SingerAlg.ProposedPermutationIsomorphism( <data1>, <data2> )
##
##  <#GAPDoc Label="SingerAlg.ProposedPermutationIsomorphism">
##  <ManSection>
##  <Func Name="SingerAlg.ProposedPermutationIsomorphism"
##   Arg='data1, data2'/>
##
##  <Returns>
##  a string, a permutation, or <K>fail</K>.
##  </Returns>
##  <Description>
##  Let <A>data1</A> and <A>data2</A> be the
##  <Ref Func="LoewyStructureInfo" Label="for a Singer algebra"/> values
##  of two Singer algebras <M>A_1</M>, <M>A_2</M>, respectively,
##  of the same dimension <M>z+1</M>, say.
##  <P/>
##  This function returns either a string that describes why there is no
##  algebra isomorphism induced by mapping the canonical basis of <M>A_1</M>
##  to the permuted canonical basis of <M>A_2</M>,
##  or <K>fail</K> (indicating that the relevant functionality of the
##  <Package>GraPe</Package> package is not available),
##  or a permutation <M>\pi</M> such that at least the following necessary
##  conditions for such a mapping are satisfied.
##  <P/>
##  <List>
##  <Item>
##    <M>\pi</M> maps pairs <M>(i,j)</M> where <M>b_i \cdot b_j</M> is zero
##    to pairs <M>(k,l)</M> where <M>B_k \cdot B_l</M> is zero.
##  </Item>
##  <Item>
##    The basis vectors <M>b_i</M> and <M>B_{{\pi(i)}}</M> lie in the same
##    Loewy layer and in the same socle layer.
##  </Item>
##  <Item>
##    The basis vector <M>b_i</M> is a <M>p^m</M>-th power if and only if
##    <M>B_{{\pi(i)}}</M> is a <M>p^m</M>-th power.
##  </Item>
##  </List>
##  <P/>
##  The first condition is forced via a call to the function
##  <Ref Func="GraphIsomorphism" BookName="grape"/>
##  (which is based on <Cite Key="Nau90"/>)
##  from the &GAP; package <Package>GraPe</Package> <Cite Key="GRAPE"/> with
##  two graphs whose incidence relation is defined by this property,
##  and the other conditions are translated into colorings of these graphs.
##  <P/>
##  When a permutation is returned, one can check with
##  <Ref Func="SingerAlg.IsInducedAlgebraIsomorphism"/>
##  whether it does in fact induce an algebra isomorphism.
##  (For Singer algebras <M>A[q,z]</M> with <M>z \leq &MAXZ;</M>,
##  this is always the case, see Section <Ref Subsect="subsect:permiso"/>.)
##  <P/>
##  <Example><![CDATA[
##  gap> # a canonical isomorphism
##  gap> data1:= LoewyStructureInfo( 3, 7 );;
##  gap> data2:= LoewyStructureInfo( 6, 7 );;
##  gap> SingerAlg.ProposedPermutationIsomorphism( data1, data2 );
##  ()
##  gap> # a proper permutation isomorphism
##  gap> data1:= LoewyStructureInfo(  41, 275 );;
##  gap> data2:= LoewyStructureInfo( 116, 275 );;
##  gap> pi:= SingerAlg.ProposedPermutationIsomorphism( data1, data2 );;
##  gap> t1:= SingerAlg.MultTable( data1 );;
##  gap> t2:= SingerAlg.MultTable( data2 );;
##  gap> SingerAlg.IsInducedAlgebraIsomorphism( t1, t2, pi );
##  true
##  gap> # no permutation isomorphism
##  gap> data1:= LoewyStructureInfo(  11, 171 );;
##  gap> data2:= LoewyStructureInfo(  68, 171 );;
##  gap> SingerAlg.ProposedPermutationIsomorphism( data1, data2 );
##  "different distributions of products"
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
##  Test the necessary condition that the two overflow matrices define
##  isomorphic graphs, using nauty;
##  if the graphs are nonisomorphic then the two algebras are not
##  permutation isomorphic;
##
##  In order to speed up the call of nauty,
##  we first compute partitions (colourings) of the vertex sets
##  that must be respected by the graph isomorphism;
##  without that, the runtime and space requirements are too large.
##
##  (Still the construction of the nauty input seems to require a huge
##  amount of space.
##  For example, z = 7866 yields an overflow at a 'Graph(...)' command;
##  however, dreadnaut itself does later not grow that much.)
##
##  The idea of the colouring is as follows.
##  - 1 must be mapped to 1 (identity element),
##    z+1 must be mapped to z+1 (socle generator).
##  - For each i, 1 < i <= z,
##    count the number of nonzero products b_i b_j (i.e., in the i-th row).
##    Any b_i can be mapped only to a b_j with the same number.
##  - The mapping must respect Loewy layers and socle layers.
##  - The mapping must respect the p-power structure,
##    that is, p^k-th powers must be mapped to p^k-th powers.
##  - Considering the above criteria simultaneously means to consider the
##    common refinement of the partitions defined by these criteria.
##  - Those b_i that are on the 1st Loewy layer and have no other nonzero
##    product than b_z can be mapped arbitrarily to b_i with the same
##    property, thus we can leave these b_i out from the graph to be
##    constructed.
##    (Well, the pair { b_i, b_{z-i} } with this property can be mapped
##    arbitrarily to any such pair.)
##
SingerAlg.ProposedPermutationIsomorphism:= function( data1, data2 )
    local z, dim, part, images, i, preims, imgs, lens, colors1, colors2,
          mat1, mat2, g1, g2, iso;

    z:= data1.parameters[3];
    dim:= z + 1;;

    part:= SingerAlg.CommonPartition( data1, data2 );
    if IsString( part ) then
      # There is a distinguishing invariant w.r.t. permutation isomorphism.
      return part;
    fi;

    # We can map all those pairs { b_i, b_{z-i} } arbitrarily
    # for which both entries do not occur as products,
    # are on the first Loewy layer,
    # and all products with other b_j are zero.
    images:= [];
    images[1]:= 1;
    images[ z+1 ]:= z+1;
    for i in [ 1 .. Length( part.freemult1 ) ] do
      if 2 * part.freemult1[i] <= z+2 then
        images[ part.freemult1[i] ]:= part.freemult2[i];
        images[ z+2-part.freemult1[i] ]:= z+2-part.freemult2[i];
      fi;
    od;

    # Find out whether we have a chance to call the standalone in question.
    if ( not IsBound( GRAPE_NAUTY ) ) or
       ( ValueGlobal( "GRAPE_NAUTY" ) and
         ValueGlobal( "GRAPE_DREADNAUT_EXE" ) = fail ) or
       ( not ValueGlobal( "GRAPE_NAUTY" ) and
         ValueGlobal( "GRAPE_BLISS_EXE" ) = fail ) then
      # Grape or its standalones are not available.
      Info( InfoSingerAlg, 1, "no Grape standalone available" );
      return fail;
    fi;

    preims:= List( part.part1, l -> Difference( l, part.freemult1 ) );
    imgs:= List( part.part2, l -> Difference( l, part.freemult2 ) );

    lens:= Filtered( List( preims, Length ), IsPosInt );
    colors1:= List( [ 1 .. Length( lens ) ],
                i -> [ 1 + Sum( lens{[1..i-1]},0) .. Sum( lens{[1..i]} ) ] );
    lens:= Filtered( List( imgs, Length ), IsPosInt );
    colors2:= List( [ 1 .. Length( lens ) ],
                i -> [ 1 + Sum( lens{[1..i-1]},0) .. Sum( lens{[1..i]} ) ] );

    preims:= Concatenation( preims );
    imgs:= Concatenation( imgs );

    mat1:= SingerAlg.MultTable( data1 );;
    mat2:= SingerAlg.MultTable( data2 );;

    # define the graphs on the submatrices of not freely mapped points
    g1:= rec( graph:= ValueGlobal( "Graph" )( Group( () ),
                             [ 1 .. Length( preims ) ],
                             OnPoints,
                             function( x, y )
                               return mat1[ preims[x], preims[y] ] <> 0;
                             end, true ),
              colourClasses:= colors1 );;

    g2:= rec( graph:= ValueGlobal( "Graph" )( Group( () ),
                             [ 1 .. Length( imgs ) ],
                             OnPoints,
                             function( x, y )
                               return mat2[ imgs[x], imgs[y] ] <> 0;
                             end, true ),
              colourClasses:= colors2 );;

    iso:= ValueGlobal( "GraphIsomorphism" )( g1, g2 );
    if iso = fail then
      # no graph isomorphism -- no permutation isomorphism
      return "no graph isomorphism";
    fi;

    # construct the proposed permutation on the whole basis
    for i in [ 1 .. Length( preims ) ] do
      images[ preims[i] ]:= imgs[ i^iso ];
    od;

    return PermList( images );
end;


############################################################################
##
#F  SingerAlg.IsInducedAlgebraIsomorphism( <t1>, <t2>, <perm> )
##
##  <#GAPDoc Label="SingerAlg.IsInducedAlgebraIsomorphism">
##  <ManSection>
##  <Func Name="SingerAlg.IsInducedAlgebraIsomorphism" Arg='t1, t2, perm'/>
##
##  <Returns>
##  <K>true</K> or <K>false</K>.
##  </Returns>
##  <Description>
##  Let <A>t1</A> and <A>t2</A> be the multiplication tables
##  of two Singer algebras <M>A_1</M>, <M>A_2</M>, respectively,
##  of the same dimension <M>z+1</M>, say,
##  as returned by <Ref Func="SingerAlg.MultTable"/>.
##  Let <A>perm</A> be a permutation with largest moved point
##  at most <M>z</M>.
##  <P/>
##  This function returns <K>true</K> if mapping the <M>i</M>-th vector
##  of the canonical basis of <M>A_1</M>
##  to the <M>i</M><C>^</C><A>perm</A>-th vector of the canonical basis of
##  <M>A_2</M> defines an algebra isomorphism,
##  and <K>false</K> otherwise.
##  <P/>
##  <Example><![CDATA[
##  gap> # Loewy length 3, naturally isomorphic
##  gap> t1:= SingerAlg.MultTable( LoewyStructureInfo( 3, 7 ) );;
##  gap> t2:= SingerAlg.MultTable( LoewyStructureInfo( 6, 7 ) );;
##  gap> SingerAlg.IsInducedAlgebraIsomorphism( t1, t2, () );
##  true
##  gap> # q1 and q2 generate the same group of residues modulo z,
##  gap> # naturally isomorphic
##  gap> t1:= SingerAlg.MultTable( LoewyStructureInfo( 2, 7 ) );;
##  gap> t2:= SingerAlg.MultTable( LoewyStructureInfo( 4, 7 ) );;
##  gap> SingerAlg.IsInducedAlgebraIsomorphism( t1, t2, () );
##  true
##  gap> # one of the explicitly computed natural isomorphisms
##  gap> t1:= SingerAlg.MultTable( LoewyStructureInfo( 3, 65 ) );;
##  gap> t2:= SingerAlg.MultTable( LoewyStructureInfo( 9, 65 ) );;
##  gap> SingerAlg.IsInducedAlgebraIsomorphism( t1, t2, () );
##  true
##  gap> # one of the explicitly computed permutation isomorphisms
##  gap> # that are not natural isomorphisms
##  gap> t1:= SingerAlg.MultTable( LoewyStructureInfo(  41, 275 ) );;
##  gap> t2:= SingerAlg.MultTable( LoewyStructureInfo( 116, 275 ) );;
##  gap> SingerAlg.IsInducedAlgebraIsomorphism( t1, t2, () );
##  false
##  gap> pi:= (2,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3)
##  > (28,29,30,31,32)(38,39,40,41,42)(43,44,45,46,47,48,49,50)
##  > (54,55,57,79,77,94,91,89,87,83,82,81,80,78,76,75,74,73,72,64,65,63,62,
##  > 61,60)
##  > (85,99,97,93,90,88,86)(92,108,107,106,105,98,96,95)
##  > (109,110,112,113,115)
##  > (114,116,117,120,118,121,124,127,119,122,125,129,132,149,146,144,143,
##  > 142,141,140,154,151,147,163,161,160,157,159,156,153,150,158,155,152,
##  > 148,145,128,131,133,134,135,136,137,123,126,130)(162,168,167,165,164)
##  > (169,170,171,172,179,181,182,185)(178,180,184,187,189,191,192)
##  > (183,186,188,190,194,195,196,197,199,201,202,203,204,205,213,212,214,
##  > 215,216,217,223,222,220,198,200)(227,234,233,232,231,230,229,228)
##  > (235,239,238,237,236)(245,249,248,247,246)
##  > (253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,
##  > 270,271,272,273,274,275);;
##  gap> SingerAlg.IsInducedAlgebraIsomorphism( t1, t2, pi );
##  true
##  gap> SingerAlg.IsInducedAlgebraIsomorphism( t1, t2, pi^-1 );
##  false
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.IsInducedAlgebraIsomorphism:= function( t1, t2, perm )
    local i, ipi, j, jpi, e1, e2;

    for i in [ 1 .. Length( t1 ) ] do
      ipi:= i^perm;
      for j in [ 1 .. i ] do
        jpi:= j^perm;
        e1:= t1[ i, j ];
        e2:= t2[ ipi, jpi ];
        if not ( ( e1 = 0 and e2 = 0 ) or ( e1 <> 0 and e1^perm = e2 ) ) then
          return false;
        fi;
      od;
    od;

    return true;
end;


#############################################################################
##
#E

