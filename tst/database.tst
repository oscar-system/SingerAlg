#############################################################################
##
#W  database.tst        GAP package SingerAlg                   Thomas Breuer
##
#Y  Copyright (C)  2020,  Lehrstuhl D für Mathematik,   RWTH Aachen,  Germany
##

gap> START_TEST( "database.tst");

##
gap> LoadPackage( "SingerAlg", false );
true

## This test runs several minutes.
## It is shown in the manual but does not belong to the manual tests.
gap> ForAll( [ 1 .. SingerAlg.MaxZ ],
>            z -> Length( AllSingerAlgebraInfos( "z", z ) ) =
>                 Sum( PrimeResidues( z ),
>                      q -> 1 / Phi( OrderMod( q, z ) ) ) );
true

##
gap> OneSingerAlgebraInfo().z;
1
gap> OneSingerAlgebraInfo( "z" );
Error, <condsvals> must have an even number of entries
gap> OneSingerAlgebraInfo( "z", 7 ).z;
7

## Check some of the stored permutation isomorphisms.
gap> joins:= SingerAlg.ContentsOfDataFile( "joinsPermExt.json" )[2];;
gap> for entry in joins{ [ 1 .. 50 ] } do
>      z:= entry[1];
>      q1:= entry[2];
>      q2:= entry[3];
>      pi:= PermList( entry[4] );
>      data1:= LoewyStructureInfo( q1, z );
>      data2:= LoewyStructureInfo( q2, z );
>      decoded:= SingerAlg.PermutationFromReducedPermutation(
>                    data1, data2, pi );
>      T1:= SingerAlg.MultTable( data1 );
>      T2:= SingerAlg.MultTable( data2 );
>      if not SingerAlg.IsInducedAlgebraIsomorphism( T1, T2, decoded ) then
>        Error( [ z, q1, q2, pi ] );
>      fi;
>    od;

##
gap> STOP_TEST( "database.tst" );

#############################################################################
##
#E

