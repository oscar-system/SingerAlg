#############################################################################
##
#W  mindeg.gd            GAP 4 package SingerAlg                Thomas Breuer
##
##  This file contains the declarations for GAP functions related to
##  the numbers <M>m(q,e)</M>.
##


#############################################################################
##
#A  SingerAlgE( <A> )
#O  SingerAlgE( <q>, <n>, <z> )
##
##  Apparently it is sometimes useful to switch between <z> and <e>,
##  where <e> = (<q>^<n>-1)/<z>.
##
DeclareAttribute( "SingerAlgE", IsSingerAlgebra );

DeclareOperation( "SingerAlgE", [ IsPosInt, IsPosInt ] );

DeclareOperation( "SingerAlgE", [ IsPosInt, IsPosInt, IsPosInt ] );


#############################################################################
##
#V  MinimalDegreeOfSingerAlgebraCache
##
##  'MinimalDegreeOfSingerAlgebraCache' is a record that contains
##  in component e, if bound, a record that contains in component q,
##  if bound, the value m(q,e).
##
##  (Since e may be large, we do not want to use position e in a plain list.)
##
BindGlobal( "MinimalDegreeOfSingerAlgebraCache",  rec() );


#############################################################################
##
#A  MinimalDegreeOfSingerAlgebra( <A> )
#O  MinimalDegreeOfSingerAlgebra( <q>, <e> )
#A  MinimalDegreeOfSingerAlgebraGAP( <A> )
#O  MinimalDegreeOfSingerAlgebraGAP( <q>, <e> )
#A  MinimalDegreeOfSingerAlgebraJulia( <A> )
#O  MinimalDegreeOfSingerAlgebraJulia( <q>, <e> )
##
##  <#GAPDoc Label="MinimalDegreeOfSingerAlgebra">
##  <ManSection>
##  <Heading>MinimalDegreeOfSingerAlgebra</Heading>
##  <Attr Name="MinimalDegreeOfSingerAlgebra" Arg='A'
##   Label="for a Singer algebra"/>
##  <Oper Name="MinimalDegreeOfSingerAlgebra" Arg='q, e'
##   Label="for Singer algebra parameters"/>
##  <Attr Name="MinimalDegreeOfSingerAlgebraGAP" Arg='A'
##   Label="for a Singer algebra"/>
##  <Oper Name="MinimalDegreeOfSingerAlgebraGAP" Arg='q, e'
##   Label="for Singer algebra parameters"/>
##  <Attr Name="MinimalDegreeOfSingerAlgebraJulia" Arg='A'
##   Label="for a Singer algebra"/>
##  <Oper Name="MinimalDegreeOfSingerAlgebraJulia" Arg='q, e'
##   Label="for Singer algebra parameters"/>
##
##  <Returns>
##  a positive integer.
##  </Returns>
##  <Description>
##  For two coprime positive integers <A>q</A> and <A>e</A>,
##  <Ref Oper="MinimalDegreeOfSingerAlgebra"
##  Label="for Singer algebra parameters"/>
##  computes the minimal number of powers of <A>q</A> such that
##  <A>e</A> divides the sum of these powers.
##  <P/>
##  If a Singer algebra <M>A[q,z]</M> is given as the argument <A>A</A>
##  (see <Ref Func="SingerAlgebra" Label="for parameters"/>) then the value
##  for the parameters <M>q</M> and <M>e = (q^n-1)/z</M> is returned,
##  where <M>n</M> is the multiplicate order of <M>q</M> modulo <M>z</M>,
##  see <Ref Func="OrderMod" BookName="ref"/>;
##  note that the minimal degree does not depend on <M>n</M>.
##  <P/>
##  The same value is returned by <Ref Oper="MinimalDegreeOfSingerAlgebraGAP"
##  Label="for Singer algebra parameters"/> and
##  <Ref Oper="MinimalDegreeOfSingerAlgebraJulia"
##  Label="for Singer algebra parameters"/>,
##  which use implementations in &GAP; and &Julia;, respectively.
##  <Ref Oper="MinimalDegreeOfSingerAlgebra"
##  Label="for Singer algebra parameters"/> delegates to the &Julia; variant
##  if the package <Package>JuliaInterface</Package> is available,
##  and to the &GAP; variant otherwise.
##  <P/>
##  <Example><![CDATA[
##  gap> A:= SingerAlgebra( 6, 2, 7 );
##  A[6,2,7]
##  gap> MinimalDegreeOfSingerAlgebra( A );
##  5
##  gap> MinimalDegreeOfSingerAlgebra( 6, 5 );
##  5
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "MinimalDegreeOfSingerAlgebraJulia", IsSingerAlgebra );

DeclareOperation( "MinimalDegreeOfSingerAlgebraJulia", [ IsPosInt, IsPosInt ] );

DeclareOperation( "MinimalDegreeOfSingerAlgebraJulia",
    [ IsPosInt, IsPosInt, IsPosInt ] );

DeclareAttribute( "MinimalDegreeOfSingerAlgebraGAP", IsSingerAlgebra );

DeclareOperation( "MinimalDegreeOfSingerAlgebraGAP", [ IsPosInt, IsPosInt ] );

DeclareOperation( "MinimalDegreeOfSingerAlgebraGAP",
    [ IsPosInt, IsPosInt, IsPosInt ] );

if IsPackageMarkedForLoading( "JuliaInterface", "" ) then
  BindGlobal( "MinimalDegreeOfSingerAlgebra",
      MinimalDegreeOfSingerAlgebraJulia );
else
  BindGlobal( "MinimalDegreeOfSingerAlgebra",
      MinimalDegreeOfSingerAlgebraGAP );
fi;


#############################################################################
##
#F  MinimalDegreeCheapGAP( <q>, <n>, <e> )
#F  MinimalDegreeHardGAP( <q>, <n>, <e> )
##
##  These functions are used by 'MinimalDegreeOfSingerAlgebraGAP'.
##  <n> must be the multiplicative order of <q> modulo <e>.
##  (If the assertion level is at least 2 then this assumption is checked,
##  but normally this level is 1;
##  note that 'START_TEST' increases the assertion level to 2 if necessary.)
##
DeclareGlobalFunction( "MinimalDegreeCheapGAP" );

DeclareGlobalFunction( "MinimalDegreeHardGAP" );


#############################################################################
##
#E

