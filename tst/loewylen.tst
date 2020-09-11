#############################################################################
##
#W  loewylen.tst           GAP 4 package SingerAlg              Thomas Breuer
##
#Y  Copyright (C)  2020,   Lehrstuhl D für Mathematik,  RWTH Aachen,  Germany
##

gap> START_TEST( "loewylen.tst" );

##
gap> LoadPackage( "SingerAlg", false );
true

##  Test the GAP variant for small z.
##  If Julia is available then test also the Julia variant.
gap> for z in [ 1 .. 150 ] do
>      for r in AllSingerAlgebraInfos( "z", z ) do
>        if LoewyLengthGAP( r.q, r.z ) <> r.LL then
>          Error( "difference for z = ", z, " (GAP variant)" );
>        fi;
>        if IsBound( Julia ) and
>           LoewyLengthJulia( r.q, r.z ) <> r.LL then
>          Error( "difference for z = ", z, " (Julia variant)" );
>        fi;
>      od;
>    od;

##  Test that the implementation of the theoretic criteria
##  does not claim that the upper bound is attained
##  in those cases where we know that it is not attained.
gap> testfun:= r -> SufficientCriterionForLoewyBoundAttained(
>                       r.q, r.n, r.z, r.m ) <> "";;
gap> OneSingerAlgebraInfo( "z", [ 1 .. 10000 ], "diff", IsPosInt,
>        testfun, true );
fail

##  Test that the GAP and Julia implementations of
##  'SufficientCriterionForLoewyBoundAttained' yield the same results.
gap> if IsBound( Julia ) then
>      for z in [ 1 .. 1000 ] do
>        for r in AllSingerAlgebraInfos( "z", z ) do
>          if SufficientCriterionForLoewyBoundAttained(
>                 r.q, r.n, r.z, r.m ) <>
>             JuliaToGAP( IsString,
>                 Julia.SingerAlg.SufficientCriterionForLoewyBoundAttained(
>                     r.q, r.n, r.z, r.m ) ) then
>            Error( "difference for z = ", z );
>          fi;
>        od;
>      od;
>    fi;

##
gap> STOP_TEST( "loewylen.tst" );

#############################################################################
##
#E

