#############################################################################
##
#W  invar.g              GAP 4 package SingerAlg                Thomas Breuer
##


#############################################################################
##
##  Avoid some syntax errors.
##
if not IsPackageMarkedForLoading( "JuliaInterface", "" ) then
  GAPToJulia:= fail;
  Julia:= fail;
  JuliaEvalString:= fail;
  JuliaSymbol:= fail;
  JuliaToGAP:= fail;
fi;

#############################################################################
##
#F  GrayCodeSwitchIndexIterator( <n> )
##
##  Return an iterator for the sequence of those bit positions in an
##  <A>n</A>-bit Gray code where the next flip has to take place.
##  <P/>
##  For large enough <A>n</A>, the first values are
##  <M>1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 5, 1, 2, 1, \ldots</M>.
##  (This is [series A001511](https://oeis.org/A001511) of the
##  [OEIS](https://oeis.org) <Cite Key="OEIS">.)
##
BindGlobal( "GrayCodeSwitchIndexIterator", function( n )
    local nn;

    nn:= QuoInt( n+1, 2 );

    return IteratorByFunctions( rec(
      n:= n,
      dim:= nn, 
      pointer:= ListWithIdenticalEntries( nn, 0 ),

      NextIterator:= function( iter )
        local ptr, i, val;

        ptr:= iter!.pointer;
        for i in [ 1 .. iter!.dim ] do
          val:= ptr[i];
          if val = 0 then
            ptr[i]:= 1;
            return 2*i - 1;
          elif val = 1 then
            ptr[i]:= 2;
            return 2*i;
          elif val = 2 then
            ptr[i]:= 3;
            return 2*i - 1;
          else
            ptr[i]:= 0;
          fi;
        od;

        # The iterator is exhausted.
        return fail;
        end,

      IsDoneIterator:= function( iter )
        if iter!.n mod 2 = 0 then
          return ForAll( iter!.pointer, x -> x = 3 );
        else
          return ForAll( [ 1 .. iter!.dim-1 ], i -> iter!.pointer[i] = 3 )
                 and iter!.pointer[ iter!.dim ] = 1;
        fi;
        end,

      ShallowCopy:= iter -> rec( n:= iter!.n,
                                 dim:= iter!.dim,
                                 pointer:= ShallowCopy( iter!.pointer ) ),
      ) );
end );


#############################################################################
##
#F  SingerAlg.NumberOfProductsInSubspace( <data>, <J>, <I>[, <bound>] )
##
##  <#GAPDoc Label="SingerAlg.NumberOfProductsInSubspace">
##  <ManSection>
##  <Func Name="SingerAlg.NumberOfProductsInSubspace"
##   Arg='data, J, I[, bound]'/>
##
##  <Returns>
##  a pair of nonnegative integers, or <K>fail</K>.
##  </Returns>
##  <Description>
##  Let <A>data</A> be a record as returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/>
##  that describes a Singer algebra <M>A = A[q,z]</M>,
##  let <M>B = B(A)</M>,
##  and let <A>J</A>, <A>I</A> be subsets of <M>\{ 1, 2, \ldots, z+1 \}</M>,
##  describing subspaces <M>V</M> and <M>U</M> of <M>A</M> with bases
##  <M>( B_i; i \in J )</M> and <M>( B_i; i \in I )</M>, 
##  respectively.
##  <P/>
##  <Ref Func="SingerAlg.NumberOfProductsInSubspace"/> tries to compute the
##  cardinality of the set
##  <Display Mode="M">
##     X = \{ (x,y) \in U \times U; x \cdot y \in V \}.
##  </Display>
##  <P/>
##  For that, we set
##  <P/>
##  <Table Align="lcl">
##  <Row>
##  <Item><M>K</M></Item>
##  <Item><M>=</M></Item>
##  <Item>
##    <M>\{ i \in I; B_i \cdot B_j \not\in V</M> for some <M>j \in I \}</M>,
##  </Item>
##  </Row>
##  <Row>
##  <Item><M>S</M></Item>
##  <Item><M>=</M></Item>
##  <Item>
##    <M>\{ k; 0 \neq B_i \cdot B_j = B_k</M> for some
##             <M>i, j \in I \}</M>,
##  </Item>
##  </Row>
##  <Row>
##  <Item><M>D</M></Item>
##  <Item><M>=</M></Item>
##  <Item>
##    <M>\{ k \in S; B_k \not\in V \}</M>.
##  </Item>
##  </Row>
##  </Table>
##  <P/>
##  If <M>|K|</M> is larger than <A>bound</A> then <K>fail</K> is returned,
##  otherwise the list <M>[ e, o ]</M> such that <M>|X| = 2^e \cdot o</M>
##  holds.
##  The default value for <A>bound</A> is <M>15</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> data:= LoewyStructureInfoGAP( 23, 12, 259 );;
##  gap> radser:= SingerAlg.BasesOfRadicalSeries( data );;
##  gap> SingerAlg.NumberOfProductsInSubspace( data, radser[3], radser[1] );
##  [ 492, 51055 ]
##  gap> data:= LoewyStructureInfoGAP( 60, 12, 259 );;
##  gap> radser:= SingerAlg.BasesOfRadicalSeries( data );;
##  gap> SingerAlg.NumberOfProductsInSubspace( data, radser[3], radser[1] );
##  [ 493, 161051 ]
##  ]]></Example>
##  <P/>
##  Note that for
##  <M>x = \sum_{{i \in I}} x_i B_i</M> and
##  <M>y = \sum_{{j \in I}} y_j B_j</M>,
##  we have
##  <Display Mode="M">
##     x \cdot y = \sum_{{i, j \in I}} x_i y_j (B_i \cdot B_j)
##               = v + \sum_{{i, j \in K}} x_i y_j (B_i \cdot B_j)
##               = v' + \sum_{{k \in D}}
##                 ( \sum_{{i, j \in K, B_i \cdot B_j = B_k}} x_i y_j ) B_k,
##  </Display>
##  for some <M>v, v' \in V</M>,
##  thus <M>x \cdot y \in V</M> if and only if
##  <M>x' M_k y' = 0</M> holds for all <M>k \in D</M>,
##  where <M>[M_k]_{{i,j}}</M> is <M>1</M>
##  if <M>B_i \cdot B_j = B_k</M> holds,
##  and <M>0</M> otherwise, for <M>i, j \in K</M>,
##  and where <M>x'</M>, <M>y'</M> are the restrictions of <M>x, y</M>
##  to <M>K</M>.
##  Let <M>M(x')</M> be the matrix with rows <M>x' M_k</M>, <M>k \in D</M>.
##  Then
##  <Display Mode="M">
##     |X| = 4^{{|I|-|K|}} \cdot
##             \sum_{{x' \in F_2^{{|K|}}}} 2^{{|K|-rank( M(x') )}}.
##  </Display>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.NumberOfProductsInSubspace:= function( data, J, I, bound... )
    local q, monom, monV, monU, sums, facts, m, mm, sum, factset, l, outer,
          F, mats, one, pair, pos, mmats, count, iter, mat, v, e;

    if not ( ForAll( J, IsPosInt ) and ForAll( I, IsPosInt ) ) then
      Error( "<J> and <I> must be indices i (> 1) of nonzero monomials b_i" );
    fi;

    if Length( bound ) = 1 then
      bound:= bound[1];
    else
      bound:= 15;
    fi;

    q:= data.parameters[1];
    monom:= data.monomials;
    monV:= monom{ J };
    monU:= monom{ I };

    # sums:
    # the nonzero monomials $b \cdot b'$ with $b, b' \in U$
    # such that $b b' \not\in V$;
    # facts:
    # the factorizations of the monomials in 'sums' inside $U \times U$
    # factset:
    # the monomials for the set K (the monomials in U that can occur
    # as factors of nonzero products outside V)
    sums:= [];
    facts:= [];
    factset:= [];
    for m in monU do
      for mm in monU do
        sum:= m + mm;
        if Maximum( sum ) < q and not ( sum in monV ) then
          Add( facts, [ m, mm ] );
          UniteSet( factset, [ m, mm ] );
          AddSet( sums, sum );
        fi;
      od;
    od;

    l:= Length( factset );
    if l = 0 then
      # all products lie in V
      return [ 2 * Length( I ), 1 ];
    elif l > bound then
      # give up
      return fail;
    fi;

    # outer:
    # the monomials for the set D (those products in S that lie outside V,
    # i. e., for which we have to compute whether they vanish or not)
    outer:= Difference( sums, monV );

    # mats:
    # for each monomial m in outer, a matrix M with
    # M[i,j] = 1 if m = factset[i] * factset[j], and M[i,j] = 0 otherwise.
    F:= GF(2);
    mats:= List( outer, x -> NullMat( l, l, F ) );
    one:= Z(2);
    for pair in facts do
      pos:= Position( outer, Sum( pair ) );
      if pos <> fail then
        mats[ pos ][ Position( factset, pair[1] ),
                     Position( factset, pair[2] ) ]:= one;
      fi;
    od;

    # Compute the number of pairs in GF(2)^{|I|} \times GF(2)^{|I|}
    # that belong to elements in the space of (x,y) with x*y inside V.
    mmats:= List( [ 1 .. l ], i -> List( mats, m -> m[i] ) );
    count:= 2^l;
    iter:= GrayCodeSwitchIndexIterator( l );
    mat:= List( mats, m -> Zero( m[1] ) );
    for v in [ 2 .. 2^l ] do
      mat:= mat + mmats[ NextIterator( iter ) ];
      count:= count + 2^( l - RankMat( mat ) );
    od;

    # Split the number into 2-part and odd part.
    e:= 2 * ( Length( I ) - l );
    while count mod 2 = 0 do
      e:= e+1;
      count:= count / 2;
    od;

    return [ e, count ];
    end;


#############################################################################
##
#F  ConsiderInvariantsByParameters( <z>, <qs>[, <bounds>] )
##
##  <#GAPDoc Label="ConsiderInvariantsByParameters">
##  <ManSection>
##  <Func Name="ConsiderInvariantsByParameters" Arg='z, qs[, bounds]'/>
##
##  <Returns>
##  a record.
##  </Returns>
##  <Description>
##  Let <A>z</A> <M>\in \{ 1, 2, \ldots, 10000 \}</M>,
##  and <A>qs</A> be a list of prime residues modulo <A>z</A>.
##  <Ref Func="ConsiderInvariantsByParameters"/> tries to find an invariant
##  (under algebra isomorphisms) that is not equal for all Singer algebras
##  <M>A[q,</M><A>z</A><M>]</M>, for <M>q \in </M><A>qs</A>.
##  The invariants used here are the dimensions of suitable subspaces
##  or the numbers of solutions of suitable equations.
##  <P/>
##  The function returns a record with at least the components
##  <C>success</C> (with value <K>true</K> if an invariant was found that
##  distinguishes at least two algebras corresponding to <A>qs</A>,
##  and <K>false</K> otherwise) and <C>comment</C> (a string).
##  If the search was successful then the result contains also the components
##  <C>label</C> (a string that decribes the distinguishing invariant) and
##  <C>lists</C> (a partition of <A>qs</A> according to the values of this
##  invariant).
##  <P/>
##  If a record <A>bounds</A> is given then it controls how many checks are
##  performed, as follows.
##  <List>
##  <Item>
##    If the component <C>maxnumber</C> is bound then the function returns
##    a record with <C>success</C> value <K>false</K> as soon as
##    at least <C>maxnumber</C> invariants have been checked without success;
##    the default value of <C>maxnumber</C> is <M>100</M>.
##  </Item>
##  <Item>
##    If the components <C>RCdim</C> and <C>RCnum</C> are bound then the
##    function <Ref Func="SingerAlg.NumberOfProductsInSubspace"/> gets called
##    after the combinatorial invariants have been computed,
##    with the first two arguments taken from the first <C>RCnum</C> of these
##    invariants and with third argument equal to the value of <C>RCdim</C>;
##    by default, <Ref Func="SingerAlg.NumberOfProductsInSubspace"/> is not
##    called at all.
##    <P/>
##    If such a call is successful for two subspaces <M>U</M> and <M>V</M>
##    then the <C>label</C> component of the result has the form
##    <C>"RC(</C><M>U</M><C>,</C><M>V</M><C>)"</C>.
##  </Item>
##  </List>
##  <P/>
##  The following invariant subspaces of a Singer algebra <M>A</M> or its
##  reduction <M>A_p</M> modulo some prime <M>p</M>, respectively,
##  are considered.
##  <P/>
##  <List>
##  <Item>
##    The members <M>J(A)^i</M> of the radical series of <M>A</M>
##    (denoted by <C>"J^1"</C>, <C>"J^2"</C>, <M>\ldots</M> in the
##    <C>label</C> component;
##    see <Ref Func="SingerAlg.BasesOfRadicalSeries"/>),
##  </Item>
##  <Item>
##    the members <M>S(A)_i</M> of the socle series of <M>A</M>
##    (denoted by <C>"S_1"</C>, <C>"S_2"</C>, <M>\ldots</M>;
##    see <Ref Func="SingerAlg.BasesOfSocleSeries"/>),
##  </Item>
##  <Item>
##    the space of elements in <M>A_p</M> whose <M>p^m</M>-th powers are
##    zero, for primes <M>p</M> and positive integers <M>m</M>
##    (denoted by <C>"Roots(0,</C><M>p</M><C>,</C><M>m</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfPMRoots"/>),
##  </Item>
##  <Item>
##    the ideal in <M>A</M> or <M>A_p</M> that is spanned by an
##    invariant subspace <M>U</M> described in this list
##    (denoted by <C>"Ideal(</C><M>U</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfIdeal"/>),
##  </Item>
##  <Item>
##    the annihilator in <M>A</M> or <M>A_p</M> of an
##    invariant subspace <M>U</M> described in this list
##    (denoted by <C>"Annihilator(</C><M>U</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfAnnihilator"/>),
##  </Item>
##  <Item>
##    the space of <M>p^m</M>-th powers of elements in <M>U</M>,
##    for primes <M>p</M> and positive integers <M>m</M>,
##    where <M>U</M> is an invariant subspace of <M>A</M> or <M>A_p</M>
##    described in this list
##    (denoted by
##    <C>"Power(</C><M>U</M><C>,</C><M>p</M><C>,</C><M>m</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfPowers"/>),
##  </Item>
##  <Item>
##    the space of elements in <M>A_p</M> whose <M>p^m</M>-th powers are
##    in <M>U</M>,
##    for primes <M>p</M> and positive integers <M>m</M>,
##    where <M>U</M> is an invariant subspace of <M>A</M> or <M>A_p</M>
##    described in this list
##    (denoted by
##    <C>"Roots(</C><M>U</M><C>,</C><M>p</M><C>,</C><M>m</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfPMRoots"/>),
##  </Item>
##  <Item>
##    the product space of two subspaces <M>U</M>, <M>V</M> in the same
##    characteristic
##    (denoted by <C>"Prod(</C><M>U</M><C>,</C><M>V</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfProductSpace"/>),
##  </Item>
##  <Item>
##    the sum of two subspaces <M>U</M>, <M>V</M> in the same
##    characteristic
##    (denoted by <C>"Sum(</C><M>U</M><C>,</C><M>V</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfSum"/>),
##  </Item>
##  <Item>
##    the intersection of two subspaces <M>U</M>, <M>V</M> in the same
##    characteristic
##    (denoted by <C>"Intersection(</C><M>U</M><C>,</C><M>V</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfIntersection"/>),
##  </Item>
##  <Item>
##    the space of elements <M>\{ x \in J(A); x \cdot U \subseteq V \}</M>,
##    for two subspaces <M>U</M>, <M>V</M> in the same characteristic
##    (denoted by <C>"PC(</C><M>U</M><C>,</C><M>V</M><C>)"</C>;
##    see <Ref Func="SingerAlg.BasisOfPC"/>),
##  </Item>
##  </List>
##  <P/>
##  A basis for the invariant subspace can be recovered from the
##  <C>label</C> string of the result,
##  using the function <Ref Func="SingerAlg.InfoFromInvariantString"/>.
##  <P/>
##  A different kind of invariant is given by the number of solutions of
##  some equation, as computed with
##  <Ref Func="SingerAlg.NumberOfProductsInSubspace"/>,
##  the corresponding label has the form
##  <C>"RC(</C><M>U</M><C>,</C><M>V</M><C>)"</C>,
##  and the function <Ref Func="SingerAlg.InfoFromInvariantString"/>
##  can be used to compute the number of solutions.
##  <P/>
##  <Example><![CDATA[
##  gap> inv:= ConsiderInvariantsByParameters( 117, [ 29, 35 ] );
##  rec( comment := "total 12 invariants checked", 
##    label := "Prod(Annihilator(Power(J^1,2,1)),Roots(0,2,1))", 
##    lists := [ [ 29 ], [ 35 ] ], success := true )
##  gap> ConsiderInvariantsByParameters( 171, [ 11, 68 ] );
##  rec( comment := "no decision, checked 11 invariants", 
##    labels := [ "J^1", "J^2", "J^3", "S_2", "Roots(0,2,1)", 
##        "Annihilator(Roots(0,2,1))", "Power(J^1,2,1)", 
##        "Sum(Power(J^1,2,1),J^3)", 
##        "Sum(Power(J^1,2,1),Annihilator(Roots(0,2,1)))", 
##        "Annihilator(Power(J^1,2,1))", 
##        "Annihilator(Sum(Power(J^1,2,1),Annihilator(Roots(0,2,1))))" ], 
##    success := false )
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
BindGlobal( "ConsiderInvariantsByParameters", function( z, qs, bounds... )
    local maxnumber, RCdim, RCnum, use_gap, datalist, inv_funs, length_fun,
          transpose, all_empty, j_list, empty, all_j, n, subspaces, labels,
          badlabels, rows, DealWithNewEntry, usedprimes, found, i, action,
          newlabel, split, p, m, l, j, min, inv, set, failure;

    if Length( bounds ) <> 1 or not IsRecord( bounds[1] ) then
      bounds:= rec();
    else
      bounds:= bounds[1];
    fi;
    if IsBound( bounds.maxnumber ) then
      maxnumber:= bounds.maxnumber;
    else
      maxnumber:= 100;
    fi;
    if IsBound( bounds.RCdim ) then
      RCdim:= bounds.RCdim;
      if IsBound( bounds.RCnum ) then
        RCnum:= bounds.RCnum;
      else
        RCnum:= infinity;
      fi;
    else
      RCdim:= 0;
      RCnum:= 0;
    fi;

    # Compute the Loewy data records from the parameters.
    use_gap:= ( IsBound( bounds.GAP_or_Julia ) and bounds.GAP_or_Julia = "GAP" )
              or not IsBound( Julia );
    if use_gap then
      datalist:= List( qs, q -> LoewyStructureInfoGAP( q, z ) );
      inv_funs:= SingerAlg;
      length_fun:= Length;
      transpose:= TransposedMat;
      all_empty:= row -> ForAll( row, IsEmpty );
      j_list:= [ 2 .. z+1 ];
      empty:= [];
    else
      datalist:= List( qs, q -> LoewyStructureInfoJulia( q, z ) );
      inv_funs:= Julia.SingerAlg;
      length_fun:= Julia.length;
      transpose:= list -> TransposedMat( List( list,
                              l -> JuliaToGAP( IsList, l ) ) );
      all_empty:= row -> ForAll( row, l -> length_fun( l ) = 0 );
      j_list:= GAPToJulia( [ 2 .. z+1 ] );
      empty:= JuliaEvalString( "Int[]" );
    fi;
    all_j:= row -> ForAll( row, x -> x = j_list );

    n:= Length( qs );
    subspaces:= [];
    labels:= [];
    badlabels:= [];

    # Local function:
    # 'row' is a list of positions lists,
    # the i-th list describing the subset of the canonical basis
    # of the algebra given by the i-th entry in 'qs' that generates
    # the current invariant subspace, whose label is 'label'.
    # The function returns
    # - a record to be returned by the outer function
    #   if an improvement is found or if the bound on the number of
    #   invariants is exceeded,
    # - 'fail' if all subspaces are trivial or equal to 'J'
    #   (which is used as exit condition in some situations), and
    # - 'false' if no improvement was found.
    DealWithNewEntry:= function( label, row )
      local len, inv, set, pos;

      len:= Length( row );

      if maxnumber < Length( labels ) then
        return rec( success:= false,
                    comment:= Concatenation( "give up after ",
                                  String( Length( labels ) ),
                                  " invariants" ) );
      elif fail in row then
        # may happen in the root count
        Error( "this should not happen" );
      elif all_empty( row ) then
        AddSet( badlabels, label );
        return fail;
      elif not IsEmpty( subspaces ) and all_j( row ) then
        AddSet( badlabels, label );
        return fail;
      elif row in subspaces then
        # We had found these subspaces already earlier.
        AddSet( badlabels, label );
        return false;
      else
        # We have found some new spaces; announce this to the outer function.
        found:= true;
      fi;

      # Add the new info.
      Add( labels, label );
      Add( subspaces, row );

      # Compare the dimensions of the subspaces.
      inv:= List( row, length_fun );
      set:= Set( inv );
      if Length( set ) <> 1 then
        return rec( success:= true,
                    lists:= List( set, val -> qs{ Positions( inv, val ) } ),
                    label:= label,
                    comment:= Concatenation( "total ",
                                             String( Length( labels ) ),
                                             " invariants checked" ) );
      fi;

      # Perhaps the dimensions are equal
      # but the spaces themselves are equal to different spaces
      # that were already found earlier.
      pos:= List( [ 1 .. len ],
                  i -> PositionProperty( subspaces, l -> l[i] = row[i] ) );
      set:= Set( pos );
      if Length( set ) <> 1 then
# Apparently this does NOT happen at all in the checks for z <= 10000,
# so print something if we arrive here.
Print( "#I  unexpected decision by invariant ", label, "\n" );
        return rec( success:= true,
                    lists:= List( set, val -> qs{ Positions( pos, val ) } ),
                    label:= label,
                    comment:= Concatenation( "total ",
                                             String( Length( labels ) ),
                                             " invariants checked" ) );
      fi;

      # no improvement found
      return false;
    end;

    # We will consider reductions of Singer algebras modulo several primes,
    # and we have to take care that each invariant involves at most
    # one such reduction.
    usedprimes:= [];

    # Initialize the lists of invariants with J^i, ...
    rows:= transpose( List( datalist, inv_funs.BasesOfRadicalSeries ) );
    for i in [ 1 .. Length( rows ) ] do
      newlabel:= Concatenation( "J^", String( i ) );
      split:= DealWithNewEntry( newlabel, rows[i] );
      if IsRecord( split ) then
        return split;
      fi;
    od;

    # ... S_i, ...
    rows:= transpose( List( datalist, inv_funs.BasesOfSocleSeries ) );
    for i in [ 1 .. Length( rows ) ] do
      newlabel:= Concatenation( "S_", String( i ) );
      split:= DealWithNewEntry( newlabel, rows[i] );
      if IsRecord( split ) then
        return split;
      fi;
    od;

    # ... and the spaces of p^m-th roots of zero, for primes p.
    p:= 2;
    repeat
      m:= 0;
      repeat
        m:= m+1;
        l:= List( datalist, l -> inv_funs.BasisOfPMRoots( l, empty, p, m ) );
        if ForAny( l, x -> x <> j_list ) then
          AddSet( usedprimes, Concatenation( ",", String( p ), "," ) );
          newlabel:= Concatenation( "Roots(0,", String(p), ",",
                                    String(m), ")" );
          split:= DealWithNewEntry( newlabel, l );
          if IsRecord( split ) then
            return split;
          fi;
        fi;
      until ForAll( l, x -> x = j_list );
      p:= NextPrimeInt( p );
    until m = 1;

    # Recursively derive invariant subspaces from the known ones.
    repeat
      # No new space has been found yet in this round.
      found:= false;

      # Add ideals and annihilators.
      for i in [ 1 .. Length( labels ) ] do
        for action in [ [ "Ideal(", inv_funs.BasisOfIdeal ],
                        [ "Annihilator(", inv_funs.BasisOfAnnihilator ] ] do
          if not StartsWith( labels[i], action[1] ) then
            newlabel:= Concatenation( action[1], labels[i], ")" );
            if not ( newlabel in labels or newlabel in badlabels ) then
              split:= DealWithNewEntry( newlabel,
                  List( [ 1 .. n ],
                        k -> action[2]( datalist[k], subspaces[i][k] ) ) );
              if IsRecord( split ) then
                return split;
              fi;
            fi;
          fi;
        od;
      od;

      # Add p^m-th powers of entries which belong to no other prime.
      for i in [ 1 .. Length( labels ) ] do
        if not StartsWith( labels[i], "Power(" ) then
          p:= 2;
          m:= 1;
          repeat
            newlabel:= Concatenation( "Power(", labels[i], ",", String( p ),
                                      ",", String(m), ")" );
            if not ( newlabel in labels or newlabel in badlabels ) and
              ForAll( usedprimes,
                      q -> q = Concatenation( ",", String( p ), "," ) or
                           PositionSublist( labels[i], q ) = fail ) then
              split:= DealWithNewEntry( newlabel,
                          List( [ 1 .. n ],
                                k -> inv_funs.BasisOfPowers(
                                         datalist[k],
                                         subspaces[i][k], p, m ) ) );
              if IsRecord( split ) then
                return split;
              elif split = fail then
                # all entries zero
                if m > 1 then
                  p:= NextPrimeInt( p );
                  AddSet( usedprimes, Concatenation( ",", String( p ), "," ) );
                  m:= 1;
                  split:= true; # switch away from fail!
                fi;
              else
                m:= m+1;
              fi;
            else
              split:= fail;
            fi;
          until split = fail;
        fi;
      od;

      # Add p^m-th roots which belong to no other prime.
      for i in [ 1 .. Length( labels ) ] do
        if not StartsWith( labels[i], "Roots(" ) then
          p:= 2;
          m:= 1;
          repeat
            newlabel:= Concatenation( "Roots(", labels[i], ",", String( p ),
                                      ",", String(m), ")" );
            if not ( newlabel in labels or newlabel in badlabels ) and
               ForAll( usedprimes,
                       q -> q = Concatenation( ",", String( p ), "," ) or
                            PositionSublist( labels[i], q ) = fail ) then
              split:= DealWithNewEntry( newlabel,
                          List( [ 1 .. n ],
                                k -> inv_funs.BasisOfPMRoots(
                                         datalist[k],
                                         subspaces[i][k], p, m ) ) );
              if IsRecord( split ) then
                return split;
              elif split = fail then
                # all entries zero or all entries J
                if m > 1 then
                  p:= NextPrimeInt( p );
                  AddSet( usedprimes, Concatenation( ",", String( p ), "," ) );
                  m:= 1;
                  split:= true; # switch away from 'fail'
                fi;
              else
                m:= m+1;
              fi;
            else
              split:= fail;
            fi;
          until split = fail;
        fi;
      od;

      # Add product spaces, sums, intersections of known spaces.
      for i in [ 1 .. Length( labels ) ] do
        for j in [ 1 .. i-1 ] do
          for action in [ [ "Prod(", inv_funs.BasisOfProductSpace ],
                          [ "Sum(", inv_funs.BasisOfSum ],
                          [ "Intersection(", inv_funs.BasisOfIntersection ],
                          [ "PC(", inv_funs.BasisOfPC ],
                        ] do
            newlabel:= Concatenation( action[1], labels[i], ",",
                                      labels[j], ")" );
            if ( not ( newlabel in labels or newlabel in badlabels ) ) and
               ForAll( usedprimes,
                       q -> PositionSublist( labels[i], q ) = fail
                            or ForAll( usedprimes,
                                       qq -> qq = q or
                                             PositionSublist( labels[j],
                                                 qq ) = fail ) ) then
              split:= DealWithNewEntry( newlabel,
                          List( [ 1 .. n ],
                                k -> action[2]( datalist[k],
                                                subspaces[i][k],
                                                subspaces[j][k] ) ) );
              if IsRecord( split ) then
                return split;
              fi;
            fi;
          od;
        od;
      od;
    until not found;

    if 0 < RCdim then
      # Try root counts for pairs of at most 'RCnum' invariants.
      min:= Minimum( Length( labels ), RCnum );
      m:= 0;
      if use_gap then
        failure:= fail;
      else
        failure:= JuliaEvalString( "nothing" );
      fi;
      for i in [ 1 .. min ] do
        for j in [ 1 .. min ] do
          m:= m + 1;
          newlabel:= Concatenation( "RC(", labels[i], ",", labels[j], ")" );
          inv:= List( [ 1 .. n ],
                      k -> inv_funs.NumberOfProductsInSubspace( datalist[k],
                               subspaces[i][k], subspaces[j][k], RCdim ) );
          if not failure in inv then
            set:= Set( inv );
            if Length( set ) <> 1 then
              return rec(
                  success:= true,
                  lists:= List( set, val -> qs{ Positions( inv, val ) } ),
                  label:= newlabel,
                  comment:= Concatenation( "total ",
                                String( Length( labels ) ), " invariants ",
                                "plus ", String( m ), " RC pairs checked" ) );
            fi;
          fi;
        od;
      od;

      return rec( success:= false,
                  labels:= labels,
                  comment:= Concatenation( "no decision, checked ",
                                String( Length( labels ) ), " invariants ",
                                "plus ", String( m ), " RC pairs" ) );
    fi;

    # We did not find any distinguishing invariant.
    return rec( success:= false,
                labels:= labels,
                comment:= Concatenation( "no decision, checked ",
                              String( Length( labels ) ), " invariants" ) );
    end );


#############################################################################
##
#F  SingerAlg.SyntaxTreeOfInvariant( <str> )
##
##  This is an auxiliary function that takes a string <str> as occurs in the
##  'label' component of the records returned by the function
##  'ConsiderInvariantsByParameters',
##  and turns it into a record, which can be evaluated by
##  'SingerAlg.InfoFromInvariantString'.
##
SingerAlg.SyntaxTreeOfInvariant:= function( str )
    local explode, bracketpos, level, pos, pos1, pos2, i, term, j;

    # Trivial case:  No brackets occur.
    if not '(' in str then
      return str;
    fi;

    explode:= [];
    bracketpos:= [];
    level:= 0;
    pos:= 0;

    pos1:= Position( str, '(', pos );
    pos2:= Position( str, ')', pos );
    while pos2 <> fail do
      if pos1 = fail or pos2 < pos1 then
        Append( explode, SplitString( str{ [ pos+1 .. pos2-1 ] }, "", "," ) );
        level:= level - 1;
        Add( explode, -level );
        Add( bracketpos, Length( explode ) );
        pos:= pos2;
        pos2:= Position( str, ')', pos2 );
      else
        Append( explode, SplitString( str{ [ pos+1 .. pos1-1 ] }, "", "," ) );
        level:= level + 1;
        Add( explode, level );
        Add( bracketpos, Length( explode ) );
        pos:= pos1;
        pos1:= Position( str, '(', pos1 );
      fi;
    od;
    if level <> 0 then
      Error( "the brackets in <str> are not balanced" );
    fi;

    while not IsEmpty( bracketpos ) do
      for i in [ 2 .. Length( bracketpos ) ] do
        if IsBound( bracketpos[ i-1 ] ) and
           explode[ bracketpos[ i-1 ] ] -1 = - explode[ bracketpos[i] ] then
          term:= rec( fun:= explode[ bracketpos[ i-1 ] - 1 ],
                      args:= explode{ [ bracketpos[ i-1 ] + 1
                                        .. bracketpos[i] - 1] } );
          explode[ bracketpos[ i-1 ] - 1 ]:= term;
          for j in [ bracketpos[ i-1 ] .. bracketpos[i] ] do
            Unbind( explode[j] );
          od;
          Unbind( bracketpos[ i-1 ] );
          Unbind( bracketpos[i] );
          break;
        fi;
      od;
      explode:= Compacted( explode );
      bracketpos:= PositionsProperty( explode, IsInt );
    od;
    
    if Length( explode ) <> 1 then
      Error( "<explode> does not have length 1" );
    fi;

    return explode[1];
    end;


#############################################################################
##
#F  SingerAlg.InfoFromInvariantString( <data>, <str> )
##
##  <#GAPDoc Label="SingerAlg.InfoFromInvariantString">
##  <ManSection>
##  <Func Name="SingerAlg.InfoFromInvariantString" Arg='data, str'/>
##
##  <Returns>
##  a record.
##  </Returns>
##  <Description>
##  Let <A>data</A> be a &GAP; record as returned by
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/> or
##  a &Julia; dictionary as returned by
##  <Ref Oper="LoewyStructureInfoJulia" Label="for parameters"/>,
##  which describes a Singer algebra <M>A</M>, say,
##  and let <A>str</A> be a string as in the <C>label</C> component
##  of a record returned by <Ref Func="ConsiderInvariantsByParameters"/>.
##  <P/>
##  <Ref Func="SingerAlg.InfoFromInvariantString"/> returns a record
##  with one of the following components.
##  <P/>
##  <List>
##  <Mark><C>basisIndices</C>:</Mark>
##  <Item>
##    the list <M>I</M> of indices such that <M>\{ B(A)_i; i \in I \}</M>
##    is a basis of the subspace defined by <A>str</A>,
##  </Item>
##  <Mark><C>derivationsDim</C>:</Mark>
##  <Item>
##    the dimension of the algebra of derivations
##    (see <Ref Func="Derivations" BookName="ref"/>) of the algebra,
##  </Item>
##  <Mark><C>solutionCount</C>:</Mark>
##  <Item>
##    the pair <M>[ e, o ]</M> such that the number of solutions is
##    <M>2^e \cdot o</M>, where <M>o</M> is an odd number.
##  </Item>
##  </List>
##  <P/>
##  If <A>data</A> is a &GAP; record then the invariants in question are
##  computed with &GAP; functions, otherwise the &Julia; implementations
##  are used.
##  <P/>
##  <Example><![CDATA[
##  gap> SingerAlg.InfoFromInvariantString(
##  >        LoewyStructureInfoGAP( 29, 6, 117 ),
##  >        "Prod(Annihilator(Power(J^1,2,1)),Roots(0,2,1))" );
##  rec( basisIndices := [ 36, 64, 69, 73, 80, 93, 95, 100, 101, 118 ] )
##  gap> SingerAlg.InfoFromInvariantString(
##  >        LoewyStructureInfoGAP( 35, 6, 117 ),
##  >        "Prod(Annihilator(Power(J^1,2,1)),Roots(0,2,1))" );
##  rec( basisIndices := [ 37, 51, 60, 64, 73, 77, 86, 87, 91, 100, 109, 
##        113, 118 ] )
##  gap> SingerAlg.InfoFromInvariantString(
##  >        LoewyStructureInfoGAP( 2, 3, 7 ),
##  >        "Derivations" );
##  rec( derivationsDim := 24 )
##  gap> SingerAlg.InfoFromInvariantString(
##  >        LoewyStructureInfoGAP( 23, 12, 259 ),
##  >        "RC(J^3,J^1)" );
##  rec( solutionCount := [ 492, 51055 ] )
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
SingerAlg.InfoFromInvariantString:= function( data, str )
    local labels, subspaces, inv_funs, list, empty, rows, tree, evl, result;

    labels:= [];
    subspaces:= [];

    if IsRecord( data ) then
      inv_funs:= SingerAlg;
      list:= IdFunc;
      empty:= [];
    else
      inv_funs:= Julia.SingerAlg;
      list:= l -> JuliaToGAP( IsList, l, false );
      empty:= JuliaEvalString( "Int[]" );
    fi;
      
    # the initial subspaces
    rows:= list( inv_funs.BasesOfRadicalSeries( data ) );
    Append( subspaces, rows );
    Append( labels, List( [ 1 .. Length( rows ) ],
                          i -> Concatenation( "J^", String(i) ) ) );
    
    rows:= list( inv_funs.BasesOfSocleSeries( data ) );
    Append( subspaces, rows );
    Append( labels, List( [ 1 .. Length( rows ) ],
                          i -> Concatenation( "S_", String(i) ) ) );

    # create the tree
    tree:= SingerAlg.SyntaxTreeOfInvariant( str );

    # evaluate recursively
    evl:= function( tree )
      local pos, paras, args, fun, res;

      if IsString( tree ) then
        pos:= Position( labels, tree );
        if pos <> fail then
          return subspaces[ pos ];
        elif ForAll( tree, IsDigitChar ) then
          return Int( tree );
        elif tree = "Derivations" then
          # This can occur only on the outermost level.
          if IsRecord( data ) then
            paras:= data.parameters;
          else
            # We have no Julia variant for the computations.
            paras:= Julia.Base.get( data, JuliaSymbol( "parameters" ), fail );
          fi;
          return rec( derivationsDim:= Dimension( Derivations(
                          CanonicalBasis( SingerAlgebra(
                              paras[1], paras[3], GF(2) ) ) ) ) );
        fi;
      elif IsRecord( tree ) then
        args:= List( tree.args, evl );
        fun:= tree.fun;
        if fun = "Ideal" then
          return inv_funs.BasisOfIdeal( data, args[1] );
        elif fun = "Annihilator" then
          return inv_funs.BasisOfAnnihilator( data, args[1] );
        elif fun = "Power" then
          return inv_funs.BasisOfPowers( data, args[1], args[2], args[3] );
        elif fun = "Roots" then
          if args[1] = 0 then
            args[1]:= empty;
          fi;
          return inv_funs.BasisOfPMRoots( data, args[1], args[2], args[3] );
        elif fun = "Prod" then
          return inv_funs.BasisOfProductSpace( data, args[1], args[2] );
        elif fun = "Sum" then
          return inv_funs.BasisOfSum( data, args[1], args[2] );
        elif fun = "Intersection" then
          return inv_funs.BasisOfIntersection( data, args[1], args[2] );
        elif fun = "PC" then
          return inv_funs.BasisOfPC( data, args[1], args[2] );
        elif fun = "RC" then
          # This can occur only on the outermost level.
          res:= inv_funs.NumberOfProductsInSubspace(
                    data, args[1], args[2], SingerAlg.MaxZ );
          if not IsList( res ) then
            res:= JuliaToGAP( IsList, res, true );
          fi;
          return rec( solutionCount:= res );
        else
          Error( "not supported invariant '", fun, "'" );
        fi;
      fi;
      Error( "not supported input '", tree, "'" );
    end;

    result:= evl( tree );
    if IsList( result ) then
      result:= rec( basisIndices:= result );
    elif not IsRecord( result ) then
      result:= rec( basisIndices:= JuliaToGAP( IsList, result ) );
    fi;
    return result;
end;


if not IsPackageMarkedForLoading( "JuliaInterface", "" ) then
  Unbind( GAPToJulia );
  Unbind( Julia );
  Unbind( JuliaEvalString );
  Unbind( JuliaSymbol );
  Unbind( JuliaToGAP );
fi;


#############################################################################
##
#E
