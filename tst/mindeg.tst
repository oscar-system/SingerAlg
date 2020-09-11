#############################################################################
##
#W  mindeg.tst             GAP 4 package SingerAlg              Thomas Breuer
##
#Y  Copyright (C)  2020,   Lehrstuhl D für Mathematik,  RWTH Aachen,  Germany
##

gap> START_TEST( "mindeg.tst" );

##
gap> LoadPackage( "SingerAlg", false );
true

##  Test the GAP variant for small z.
##  If Julia is available then test also the Julia variant.
gap> for z in [ 1 .. 120 ] do
>      for r in AllSingerAlgebraInfos( "z", z ) do
>        if MinimalDegreeOfSingerAlgebraGAP( r.q, r.e ) <> r.m then
>          Error( "difference for z = ", z, " (GAP variant)" );
>        fi;
>        if IsBound( Julia ) and
>           MinimalDegreeOfSingerAlgebraJulia( r.q, r.e ) <> r.m then
>          Error( "difference for z = ", z, " (Julia variant)" );
>        fi;
>      od;
>    od;

##
gap> STOP_TEST( "mindeg.tst" );

#############################################################################
##
#E

