#############################################################################
##
#W  loewy.gd             GAP 4 package SingerAlg                Thomas Breuer
##
##  This file contains declarations of GAP functions for studying
##  the Loewy structure of Singer algebras A[q,z].
##


#############################################################################
##
##  Declare the necessary filters and operations.
##


#############################################################################
##
#V  InfoSingerAlg
##
##  Currently this is used for checking when data files get evaluated.
##
DeclareInfoClass( "InfoSingerAlg" );


#############################################################################
##
#V  SingerAlg
##
##  <#GAPDoc Label="SingerAlg">
##  <ManSection>
##  <Var Name="SingerAlg"/>
##
##  <Description>
##  This global record is used to store information about the
##  <Package>SingerAlg</Package> package.
##  Some of its components are documented individually, see the manual index.
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
BindGlobal( "SingerAlg", rec() );


#############################################################################
##
#C  IsSingerAlgebra( <A> )
##
##  <#GAPDoc Label="IsSingerAlgebra">
##  <ManSection>
##  <Filt Name="IsSingerAlgebra" Arg='A' Type="Category"/>
##
##  <Description>
##  This filter is set in all algebras constructed with
##  <Ref Func="SingerAlgebra" Label="for parameters"/>.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 6, 7 );
##  A[6,2,7]
##  gap> IsSingerAlgebra( A );
##  true
##  gap> AA:= AlgebraByStructureConstants( Rationals,
##  >             StructureConstantsTable( Basis( A ) ) );
##  <algebra of dimension 8 over Rationals>
##  gap> IsSingerAlgebra( AA );
##  false
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareCategory( "IsSingerAlgebra",
    IsAlgebraWithOne and IsAbelian and IsAssociative );


#############################################################################
##
#A  ParametersOfSingerAlgebra( <A> )
##
##  <#GAPDoc Label="ParametersOfSingerAlgebra">
##  <ManSection>
##  <Attr Name="ParametersOfSingerAlgebra" Arg='A'/>
##
##  <Description>
##  For a Singer algebra <A>A</A><M> = A[q,n,z]</M>
##  (see <Ref Func="SingerAlgebra" Label="for parameters"/>),
##  the value is the list <M>[ q, n, z ]</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 6, 7 );
##  A[6,2,7]
##  gap> ParametersOfSingerAlgebra( A );
##  [ 6, 2, 7 ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "ParametersOfSingerAlgebra", IsSingerAlgebra );


#############################################################################
##
#A  LoewyStructureInfo( <A> )
#A  LoewyStructureInfoGAP( <A> )
#A  LoewyStructureInfoJulia( <A> )
#O  LoewyStructureInfo( <q>[, <n>], <z> )
#O  LoewyStructureInfoGAP( <q>[, <n>], <z> )
#O  LoewyStructureInfoJulia( <q>[, <n>], <z> )
##
##  <#GAPDoc Label="LoewyStructureInfo">
##  <ManSection>
##  <Heading>LoewyStructureInfo</Heading>
##  <Attr Name="LoewyStructureInfo" Arg='A' Label="for a Singer algebra"/>
##  <Attr Name="LoewyStructureInfoGAP" Arg='A' Label="for a Singer algebra"/>
##  <Attr Name="LoewyStructureInfoJulia" Arg='A' Label="for a Singer algebra"/>
##  <Oper Name="LoewyStructureInfo" Arg='q[,n],z' Label="for parameters"/>
##  <Oper Name="LoewyStructureInfoGAP" Arg='q[,n],z' Label="for parameters"/>
##  <Oper Name="LoewyStructureInfoJulia" Arg='q[,n],z' Label="for parameters"/>
##
##  <Description>
##  For a Singer algebra <A>A</A>
##  (see <Ref Func="SingerAlgebra" Label="for parameters"/>)
##  with parameters <A>q</A>, <A>n</A>, <A>z</A>,
##  or for these parameters themselves,
##  these three operations compute the distribution of the canonical basis
##  in <M>A[<A>q</A>,<A>z</A>]</M> to Loewy layers,
##  using the algorithm from <Cite Key="BHHK1" Where="Proposition 3.2"/>.
##  <P/>
##  Let <M>e = (<A>q</A>^{{<A>n</A>}}-1)/<A>z</A></M>.
##  <P/>
##  <Ref Oper="LoewyStructureInfoJulia" Label="for parameters"/> returns
##  a &Julia; dictionary whose keys are the following symbols.
##  <P/>
##  <List>
##  <Mark><C>:monomials</C></Mark>
##  <Item>
##     the array of reversed (see <Ref Func="CoefficientsQadicReversed"/>)
##     <A>q</A>-adic expansions for multiples of <M>e</M>,
##     each of length <A>n</A>,
##  </Item>
##  <Mark><C>:layers</C></Mark>
##  <Item>
##     the array of the Loewy layers to which the monomials belong,
##  </Item>
##  <Mark><C>:chain</C></Mark>
##  <Item>
##     an array of positions of monomials of a longest ascending chain,
##  </Item>
##  <Mark><C>:m</C></Mark>
##  <Item>
##     the value <M>m(<A>q</A>, e)</M>
##     (see <Ref Attr="MinimalDegreeOfSingerAlgebra"
##     Label="for a Singer algebra"/>
##  </Item>
##  <Mark><C>:LL</C></Mark>
##  <Item>
##     the Loewy length of <A>A</A>
##     (see <Ref Attr="LoewyLength" Label="for a Singer algebra"/>),
##     equal to the length of the <C>:layers</C> value plus <M>1</M>,
##  </Item>
##  <Mark><C>:parameters</C></Mark>
##  <Item>
##     the array <C>[ </C><A>q</A><C>, </C><A>n</A><C>, </C><A>z</A><C> ]</C>
##     of parameters of <A>A</A>.
##  </Item>
##  </List>
##  <P/>
##  <Ref Oper="LoewyStructureInfoGAP" Label="for parameters"/> returns
##  a &GAP; record whose components correspond to the keys listed above.
##  <Ref Oper="LoewyStructureInfo" Label="for parameters"/> returns
##  the same; however, if &Julia; is available then this result is computed
##  by converting the result of
##  <Ref Oper="LoewyStructureInfoJulia" Label="for parameters"/> to a
##  &GAP; object.
##  <P/>
##  <Example><![CDATA[
##  gap> LoewyStructureInfo( 6, 7 );
##  rec( LL := 3, chain := [ 8, 2, 1 ], 
##    layers := [ 0, 1, 1, 1, 1, 1, 1, 2 ], m := 5, 
##    monomials := [ [ 0, 0 ], [ 0, 5 ], [ 1, 4 ], [ 2, 3 ], [ 3, 2 ], 
##        [ 4, 1 ], [ 5, 0 ], [ 5, 5 ] ], parameters := [ 6, 2, 7 ] )
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "LoewyStructureInfoJulia", IsSingerAlgebra );

DeclareOperation( "LoewyStructureInfoJulia", [ IsPosInt, IsPosInt ] );

DeclareOperation( "LoewyStructureInfoJulia",
    [ IsPosInt, IsPosInt, IsPosInt ] );

# This attribute must be mutable,
# we want to store the multiplication table etc. once it gets computed.
DeclareAttribute( "LoewyStructureInfoGAP", IsSingerAlgebra, "mutable" );

DeclareOperation( "LoewyStructureInfoGAP", [ IsPosInt, IsPosInt ] );

DeclareOperation( "LoewyStructureInfoGAP",
    [ IsPosInt, IsPosInt, IsPosInt ] );

if IsPackageMarkedForLoading( "JuliaInterface", "" ) then
  # We convert the Julia dictionary into a GAP record,
  # thus we need an additional attribute.
  DeclareAttribute( "LoewyStructureInfo", IsSingerAlgebra );

  DeclareOperation( "LoewyStructureInfo", [ IsPosInt, IsPosInt ] );

  DeclareOperation( "LoewyStructureInfo", [ IsPosInt, IsPosInt, IsPosInt ] );
else
  # We can just take the GAP record.
  BindGlobal( "LoewyStructureInfo", LoewyStructureInfoGAP );
fi;


#############################################################################
##
#F  CoefficientsQadicReversed( <k>, <z>, <q>, <n> )
##
##  <#GAPDoc Label="CoefficientsQadicReversed">
##  <ManSection>
##  <Func Name="CoefficientsQadicReversed" Arg='k, z, q, n'/>
##
##  <Description>
##  Let <A>k</A>, <A>z</A>, <A>q</A>, <A>n</A> be positive integers such that 
##  <M>0 \leq</M> <A>k</A> <M>\leq</M> <A>z</A>, <A>q</A> <M>&gt; 1</M>,
##  and <A>n</A> <M>&gt; 0</M>.
##  <P/>
##  This function computes the coefficients of the <A>q</A>-adic expansion of
##  <M>e =</M> <A>k</A> <M>(</M><A>q</A><C>^</C><A>n</A><M>-1)/</M><A>z</A>,
##  of length <A>n</A>, without creating this number,
##  which may be a large integer although all arguments are small
##  (see <Cite Key="BHHK2" Where="Remark 2.23 (iv)"/> and
##  section <Ref Subsect="large_e_examples"/>).
##  <P/>
##  If <M>e = v_n</M> <A>q</A><M>^0 + v_{{n-1}}</M> <A>q</A><M>^1 + \cdots
##  + v_1</M> <A>q</A><M>^{{n-1}}</M>
##  then the returned array is <M>[ v_1, v_2, \ldots, v_n ]</M>.
##  <P/>
##  <Example><![CDATA[
##  gap> CoefficientsQadicReversed( 2, 7, 6, 2 );
##  [ 1, 4 ]
##  gap> e:= (6^2-1)/7;
##  5
##  gap> CoefficientsQadic( 2*e, 6 );
##  [ 4, 1 ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareGlobalFunction( "CoefficientsQadicReversed" );


#############################################################################
##
#A  DimensionsLoewyFactors( <A> )
#A  LoewyVector( <A> )
##
##  <#GAPDoc Label="DimensionsLoewyFactors">
##  <ManSection>
##  <Attr Name="DimensionsLoewyFactors" Arg='A'/>
##  <Attr Name="LoewyVector" Arg='A'/>
##
##  <Description>
##  For a Singer algebra <A>A</A>
##  (see <Ref Oper="SingerAlgebra" Label="for parameters"/>),
##  this function returns the Loewy vector of <A>A</A>, that is,
##  the list of nonzero dimensions <M>J^{{i-1}} / J^i</M>,
##  for <M>i \geq 1</M>, where <M>J</M> is the Jacobson radical of <A>A</A>
##  and <M>J^0 = </M><A>A</A>.
##  <P/>
##  <Ref Attr="LoewyVector"/> is a synonym of
##  <Ref Attr="DimensionsLoewyFactors"/>.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 6, 2, 7 );
##  A[6,2,7]
##  gap> DimensionsLoewyFactors( A );
##  [ 1, 6, 1 ]
##  ]]></Example>
##  <P/>
##  In the &GAP; Reference Manual, this attribute is declared for groups,
##  see <Ref Attr="DimensionsLoewyFactors" BookName="ref"/>.
##  In that context, it means the dimensions of the Loewy factors of the
##  group algebra of its argument over the field with <M>p</M> elements,
##  where the argument is required to be a finite <M>p</M>-group.
##  Note that this value can be computed just from the group,
##  without constructing a group algebra.
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "DimensionsLoewyFactors", IsSingerAlgebra );

DeclareSynonymAttr( "LoewyVector", DimensionsLoewyFactors );


#############################################################################
##
#F  LoewyVectorAbbreviated( <v> )
#F  LoewyVectorExpanded( <v> )
##
##  <#GAPDoc Label="LoewyVectorAbbreviated">
##  <ManSection>
##  <Heading>LoewyVectorAbbreviated and LoewyVectorExpanded</Heading>
##  <Func Name="LoewyVectorAbbreviated" Arg='v'/>
##  <Func Name="LoewyVectorExpanded" Arg='v'/>
##
##  <Description>
##  For a dense list <A>v</A> of positive integers,
##  <Ref Func="LoewyVectorAbbreviated"/> returns a new list in which each
##  maximal sublist of at least two consecutive equal entries is replaced
##  by the list containing this element and its multiplicity.
##  <P/>
##  For a dense list <A>v</A> whose entries are non-lists and pairs whose
##  second entries are positive integers,
##  <Ref Func="LoewyVectorExpanded"/> returns a new list in which each such
##  pair is replaced by a sublist that contains the first entry with
##  multiplicity given by the second entry.
##  <P/>
##  <Example><![CDATA[
##  gap> LoewyVectorAbbreviated( [ 1, 1, 1, 1 ] );
##  [ [ 1, 4 ] ]
##  gap> LoewyVectorAbbreviated( [ 1, 7, 7, 3, 3, 1, 1, 1 ] );
##  [ 1, [ 7, 2 ], [ 3, 2 ], [ 1, 3 ] ]
##  gap> LoewyVectorExpanded( [ [ 1, 4 ] ] );
##  [ 1, 1, 1, 1 ]
##  gap> LoewyVectorExpanded( [ 1, [ 7, 2 ], [ 3, 2 ], [ 1, 3 ] ] );
##  [ 1, 7, 7, 3, 3, 1, 1, 1 ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareGlobalFunction( "LoewyVectorAbbreviated" );
DeclareGlobalFunction( "LoewyVectorExpanded" );


#############################################################################
##
#A  LoewyLength( <A> )
#O  LoewyLength( <q>, <z> )
#O  LoewyLength( <q>, <n>, <z>, <m> )
##
##  <#GAPDoc Label="LoewyLength">
##  <ManSection>
##  <Heading>LoewyLength</Heading>
##  <Attr Name="LoewyLength" Arg='A' Label="for a Singer algebra"/>
##  <Oper Name="LoewyLength" Arg='q[, n], z[, m]'
##   Label="for Singer algebra parameters"/>
##  <Attr Name="LoewyLengthGAP" Arg='A' Label="for a Singer algebra"/>
##  <Oper Name="LoewyLengthGAP" Arg='q[, n], z[, m]'
##   Label="for Singer algebra parameters"/>
##  <Attr Name="LoewyLengthJulia" Arg='A' Label="for a Singer algebra"/>
##  <Oper Name="LoewyLengthJulia" Arg='q[, n], z[, m]'
##   Label="for Singer algebra parameters"/>
##
##  <Description>
##  Let <A>q</A>, <A>n</A>, <A>z</A> be positive integers such that <A>z</A>
##  divides <A>q</A><C>^</C><A>n</A> - 1;
##  the default for <A>n</A> is the multiplicative order of <A>q</A> modulo
##  <A>z</A>.
##  These functions return the Loewy length of the Singer algebra
##  <M>A[ </M><A>q</A><M>, </M><A>n</A><M>, </M><A>z</A><M> ]</M>,
##  see <Ref Func="SingerAlgebra" Label="for parameters"/>.
##  <P/>
##  Alternatively, also a Singer algebra <A>A</A> can be given
##  as an argument.
##  <P/>
##  Note that it may be cheap to compute the Loewy length of this algebra,
##  using criteria from <Cite Key="BHHK1"/> and <Cite Key="BHHK2"/>,
##  even if computing its Loewy vector
##  (see <Ref Attr="DimensionsLoewyFactors"/>) would be hard.
##  <P/>
##  If &Julia; is available then
##  <Ref Attr="LoewyLength" Label="for a Singer algebra"/> uses
##  <Ref Attr="LoewyLengthJulia" Label="for a Singer algebra"/>,
##  otherwise it uses
##  <Ref Attr="LoewyLengthGAP" Label="for a Singer algebra"/>.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 6, 2, 7 );
##  A[6,2,7]
##  gap> LoewyLength( A );
##  3
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "LoewyLengthJulia", IsSingerAlgebra );

DeclareOperation( "LoewyLengthJulia", [ IsPosInt, IsPosInt ] );

DeclareOperation( "LoewyLengthJulia",
    [ IsPosInt, IsPosInt, IsPosInt, IsPosInt ] );

DeclareAttribute( "LoewyLengthGAP", IsSingerAlgebra );

DeclareOperation( "LoewyLengthGAP", [ IsPosInt, IsPosInt ] );

DeclareOperation( "LoewyLengthGAP",
    [ IsPosInt, IsPosInt, IsPosInt, IsPosInt ] );

if IsPackageMarkedForLoading( "JuliaInterface", "" ) then
  BindGlobal( "LoewyLength",
      LoewyLengthJulia );
else
  BindGlobal( "LoewyLength",
      LoewyLengthGAP );
fi;


#############################################################################
##
#F  LoewyLengthCheapGAP( <q>, <n>, <z>, <m> )
#F  LoewyLengthHardGAP( <q>, <n>, <z>, <m> )
##
##  These functions are used by 'LoewyLengthGAP'.
##
DeclareGlobalFunction( "LoewyLengthCheapGAP" );

DeclareGlobalFunction( "LoewyLengthHardGAP" );


#############################################################################
##
#F  OrderModExt( <n>, <m>[, <bound>] )
##
##  <#GAPDoc Label="OrderModExt">
##  <ManSection>
##  <Func Name="OrderModExt" Arg='n, m[, bound]'/>
##
##  <Returns>
##  a nonnegative integer.
##  </Returns>
##  <Description>
##  <Index>multiplicative order of an integer</Index>
##  When called with two arguments <A>n</A> and <A>m</A>,
##  <Ref Func="OrderModExt"/> returns the same as the &GAP; library function
##  <Ref Func="OrderMod" BookName="ref"/>, that is,
##  the multiplicative order of the integer
##  <A>n</A> modulo the positive integer <A>m</A>.
##  If <A>n</A> and <A>m</A> are not coprime the order of <A>n</A> is not
##  defined and <Ref Func="OrderModExt"/> will return <C>0</C>.
##  <P/>
##  If <A>n</A> and <A>m</A> are relatively prime the multiplicative order of
##  <A>n</A> modulo <A>m</A> is the smallest positive integer <M>i</M>
##  such that  <M><A>n</A>^i \equiv 1 \pmod{<A>m</A>}</M>.
##  <P/>
##  If no a priori known multiple <A>bound</A> of the desired order is given,
##  <Ref Func="OrderModExt"/> usually spends most of its time factoring
##  <A>m</A> for computing a default for <A>bound</A>,
##  and then factoring <A>bound</A>.
##  Thus it is advisable to enter <A>bound</A> whenever one knows
##  a reasonable bound.
##  <P/>
##  If an incorrect <A>bound</A> is given then the result will be wrong.
##  <P/>
##  <Example><![CDATA[
##  gap> OrderModExt( 2, 7 );
##  3
##  gap> OrderModExt( 3, 7 );  # 3 is a primitive root modulo 7
##  6
##  gap> m:= (5^166-1) / 167;;      # about 10^113
##  gap> OrderModExt( 5, m, 166 );  # needs minutes without third argument
##  166
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareGlobalFunction( "OrderModExt" );


#############################################################################
##
#F  SufficientCriterionForLoewyBoundAttained( <q>, <n>, <z>, <m> )
##
##  <#GAPDoc Label="SufficientCriterionForLoewyBoundAttained">
##  <ManSection>
##  <Func Name="SufficientCriterionForLoewyBoundAttained" Arg='q, n, z, m'/>
##
##  <Returns>
##  a string.
##  </Returns>
##  <Description>
##  Let <A>q</A>, <A>n</A>, <A>z</A>, <A>m</A> be positive integers,
##  where <A>q</A> and <A>z</A> are coprime.
##  <Ref Func="SufficientCriterionForLoewyBoundAttained"/> returns a string
##  that describes the criterion from <Cite Key="BHHK1"/> or
##  <Cite Key="BHHK2"/> from which it follows that the Loewy length of the
##  algebra <M>A[</M><A>q</A>,<A>z</A><M>]</M> is equal to the upper bound
##  <M>\lfloor <A>n</A> (<A>q</A>-1) / <A>m</A> \rfloor + 1</M>, where
##  <A>n</A><M> = </M><C>OrderMod(</C><A>q</A><C>, </C><A>z</A><C>)</C>,
##  <M>e = (<A>q</A>^{<A>n</A>}-1) / <A>z</A></M>,
##  and <M>m = m(<A>q</A>, e)</M>;
##  if no such criterion was found then the returned string is empty.
##  <P/>
##  <Example><![CDATA[
##  gap> expls:= [ [3,2], [2,5], [3,70], [13,70], [19,70], [5,72],
##  >              [2,73], [5,76], [11,80], [13,80], [2,85], [4,123],
##  >              [3,164], [4,369], [2,771], [15,791] ];;
##  gap> for l in expls do
##  >      q:= l[1];  z:= l[2];  n:= OrderMod( q, z );  e:= (q^n-1)/z;
##  >      nn:= OrderModExt( q, e, n );
##  >      m:= MinimalDegreeOfSingerAlgebra( q, nn, e );
##  >      Print( "q = ", String( q, 2 ), ", z = ", String( z, 3 ), ":  ",
##  >      SufficientCriterionForLoewyBoundAttained( q, n, z, m ), "\n" );
##  >    od;
##  q =  3, z =   2:  Cor. I.7.1 (n <= 3)
##  q =  2, z =   5:  z < 70
##  q =  3, z =  70:  
##  q = 13, z =  70:  Thm. I.7.1
##  q = 19, z =  70:  La. I.7.1 (iii)
##  q =  5, z =  72:  La. II.4.1 for r = 1
##  q =  2, z =  73:  Prop. II.6.1 (e <= 32)
##  q =  5, z =  76:  Prop. II.5.1 (ii)
##  q = 11, z =  80:  Prop. II.5.3, II.5.6 (e | (q^n-1)/(q-1), n <= 5)
##  q = 13, z =  80:  Prop. II.3.15
##  q =  2, z =  85:  La. I.6.3
##  q =  4, z = 123:  Prop. II.5.1 (iii)
##  q =  3, z = 164:  La. II.5.2 (ii)
##  q =  4, z = 369:  Prop. II.5.1 (i)
##  q =  2, z = 771:  La. II.4.1
##  q = 15, z = 791:  Thm. II.4.3 (iii)
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareGlobalFunction( "SufficientCriterionForLoewyBoundAttained" );


#############################################################################
##
#F  SingerAlgebra( <q>[, <n>], <z>[, <R>] )
#F  SingerAlgebra( <arec>[, <R>] )
##
##  <#GAPDoc Label="SingerAlgebra">
##  <ManSection>
##  <Heading>SingerAlgebra</Heading>
##  <Func Name="SingerAlgebra" Arg='q[, n], z[, R]' Label="for parameters"/>
##  <Func Name="SingerAlgebra" Arg='arec[, R]' Label="for a record"/>
##
##  <Description>
##  For nonnegative integers <A>q</A>, <A>z</A>
##  with <A>q</A> <M>&gt; 1</M>,
##  and a field <A>R</A> (with default the field of Rationals,
##  see <Ref Var="Rationals" BookName="ref"/>),
##  let <M>n</M> be the multiplicative order of <A>q</A> modulo <A>z</A>,
##  set <M>e = (<A>q</A>^n - 1) / <A>z</A></M>,
##  and define <M>A[<A>q</A>, <A>z</A>]</M> as the free
##  <A>R</A>-module with basis <M>(b_0, b_1, \ldots, b_z)</M>
##  and multiplication defined as follows.
##  If there is no carry in the addition of the <A>q</A>-adic expansions of
##  <M>i e</M> and <M>j e</M> then <M>b_i b_j = b_{{i+j}}</M> holds,
##  and otherwise <M>b_i b_j</M> is zero.
##  <P/>
##  This function returns the algebra <M>A[<A>q</A>, <A>z</A>]</M>.
##  <P/>
##  Alternatively, a record <A>arec</A> can be given, which must have the
##  components <C>q</C> and <C>z</C> or the components <C>q</C>, <C>n</C>,
##  <C>e</C>.
##  <P/>
##  The idea is to use the algebra object first of all as a container for the
##  attributes that belong to the parameters <A>q</A> and <A>z</A>,
##  see <Ref Attr="LoewyLength" Label="for a Singer algebra"/>,
##  <Ref Attr="MinimalDegreeOfSingerAlgebra" Label="for a Singer algebra"/>,
##  and <Ref Attr="LoewyStructureInfo" Label="for a Singer algebra"/>.
##  This works well also for high dimensional algebras.
##  <P/>
##  If one really wants to do computations beyond this context,
##  for example compute with elements of the algebra,
##  then special methods for <Ref Oper="CanonicalBasis" BookName="ref"/>,
##  <Ref Oper="Representative" BookName="ref"/>,
##  <Ref Oper="GeneratorsOfAlgebra" BookName="ref"/>, or
##  <Ref Oper="GeneratorsOfAlgebraWithOne" BookName="ref"/> will trigger
##  the computation of a structure constants table,
##  and afterwards the algebra behaves like other algebras in &GAP;
##  that are defined via structure constants.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 6, 2, 7 );
##  A[6,2,7]
##  gap> Dimension( A );   # is always z+1
##  8
##  gap> LeftActingDomain( A );
##  Rationals
##  gap> A2:= SingerAlgebra( 6, 2, 7, GF(2) );
##  A[6,2,7,GF(2)]
##  gap> Print( A2, "\n" );
##  SingerAlgebra( 6, 2, 7, GF(2) )
##  gap> String( A2 );
##  "SingerAlgebra( 6, 2, 7, GF(2) )"
##  gap> SingerAlgebra( rec( q:= 6, n:= 2, e:= 5 ) );
##  A[6,2,7]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareGlobalFunction( "SingerAlgebra" );


#############################################################################
##
#A  GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( <U> )
##
##  <#GAPDoc Label="GeneratingSubsetOfCanonicalBasisOfSingerAlgebra">
##  <ManSection>
##  <Attr Name="GeneratingSubsetOfCanonicalBasisOfSingerAlgebra" Arg='U'/>
##
##  <Description>
##  Let <A>U</A> be a subspace of a Singer algebra <M>A</M>, say.
##  If this attribute is set in <A>U</A>
##  then the value is a strictly sorted list of nonnegative
##  integers such that the corresponding subset of the canonical basis of
##  <M>A</M> is a basis of <A>U</A>.
##  In particular, <A>U</A> is a combinatorial subspace of <M>A</M>,
##  see the introduction of Section
##  <Ref Sect="sect:combinatorial_structures"/>.
##  <P/>
##  There is no default method for computing the value of this attribute.
##  On the other hand, if the value is known then there are efficient
##  methods to compute annihilators, product spaces, etc.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 3, 7 );
##  A[3,6,7]
##  gap> J:= RadicalOfAlgebra( A );;
##  gap> HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( J );
##  true
##  gap> GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( J );
##  [ 2 .. 8 ]
##  gap> P:= ProductSpace( J, J );
##  <vector space of dimension 1 over Rationals>
##  gap> HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( P );
##  true
##  gap> GeneratingSubsetOfCanonicalBasisOfSingerAlgebra( P );
##  [ 8 ]
##  gap> V:= Subspace( A, Basis( A ){ [ 2, 3 ] } );;
##  gap> HasGeneratingSubsetOfCanonicalBasisOfSingerAlgebra( V );
##  false
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "GeneratingSubsetOfCanonicalBasisOfSingerAlgebra",
    IsVectorSpace );


#############################################################################
##
#A  RadicalSeriesOfAlgebra( <A> )
##
##  <#GAPDoc Label="RadicalSeriesOfAlgebra">
##  <ManSection>
##  <Attr Name="RadicalSeriesOfAlgebra" Arg='A'/>
##
##  <Description>
##  For an algebra <A>A</A> with radical <A>J</A>
##  (see <Ref Attr="RadicalOfAlgebra" BookName="ref"/>),
##  <Ref Attr="RadicalSeriesOfAlgebra"/> returns
##  the list <M>[ J^0, J^1, J^2, J^3, \ldots, J^k ]</M>,
##  where <M>k</M> is the smallest index such that <M>J^k</M> is zero.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 2, 7 );
##  A[2,3,7]
##  gap> ser:= RadicalSeriesOfAlgebra( A );
##  [ A[2,3,7], <algebra of dimension 7 over Rationals>, 
##    <algebra of dimension 4 over Rationals>, 
##    <algebra of dimension 1 over Rationals>, 
##    <algebra of dimension 0 over Rationals> ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "RadicalSeriesOfAlgebra", IsAlgebra );


#############################################################################
##
#A  SocleSeriesOfAlgebra( <A> )
##
##  <#GAPDoc Label="SocleSeriesOfAlgebra">
##  <ManSection>
##  <Attr Name="SocleSeriesOfAlgebra" Arg='A'/>
##
##  <Description>
##  For an algebra <A>A</A>,
##  <Ref Attr="SocleSeriesOfAlgebra"/> returns
##  the list <M>[ S_0, S_1, S_2, S_3, \ldots, S_k ]</M>,
##  where <M>S_0</M> is the trivial subalgebra of <A>A</A> and
##  <M>S_{{i+1}}/S_i</M> is the socle of <A>A</A><M>/S_i</M>
##  and <M>k</M> is the smallest index such that <M>S_k =</M> <A>A</A> holds.
##  (Thus <A>S_1</A> is the socle of <A>A</A>.)
##  <!-- (see <Ref Attr="SocleOfAlgebra" BookName="..."/>). -->
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 2, 7 );
##  A[2,3,7]
##  gap> ser:= SocleSeriesOfAlgebra( A );
##  [ <algebra of dimension 0 over Rationals>, 
##    <algebra of dimension 1 over Rationals>, 
##    <algebra of dimension 4 over Rationals>, 
##    <algebra of dimension 7 over Rationals>, A[2,3,7] ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "SocleSeriesOfAlgebra", IsAlgebra );


#############################################################################
##
#E

