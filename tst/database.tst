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

##
gap> STOP_TEST( "database.tst" );

#############################################################################
##
#E

