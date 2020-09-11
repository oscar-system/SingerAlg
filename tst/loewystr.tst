#############################################################################
##
#W  loewystr.tst           GAP 4 package SingerAlg              Thomas Breuer
##
#Y  Copyright (C)  2020,   Lehrstuhl D für Mathematik,  RWTH Aachen,  Germany
##

gap> START_TEST( "loewystr.tst" );

##
gap> LoadPackage( "SingerAlg", false );
true

##  Test the GAP variant for small z.
##  If Julia is available then test also the Julia variant.
gap> for z in [ 1 .. 150 ] do
>      for r in AllSingerAlgebraInfos( "z", z ) do
>        infoGAP:= LoewyStructureInfoGAP( r.q, r.z );
>        if infoGAP.LL <> r.LL or infoGAP.m <> r.m then
>          Error( "difference for z = ", z, " (GAP variant)" );
>        fi;
>        if IsBound( Julia ) and
>           LoewyStructureInfo( r.q, r.z ) <> infoGAP then
>          Error( "difference for z = ", z, " (Julia variant)" );
>        fi;
>      od;
>    od;

##
gap> STOP_TEST( "loewystr.tst" );

#############################################################################
##
#E

