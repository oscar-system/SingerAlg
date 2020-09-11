#############################################################################
##
#W  invar.tst           GAP package SingerAlg                   Thomas Breuer
##
#Y  Copyright (C)  2019,  Lehrstuhl D für Mathematik,   RWTH Aachen,  Germany
##

gap> START_TEST( "invar.tst");

#
gap> LoadPackage( "SingerAlg", false );
true

# Check that the descriptions of combinatorial invariants
# can be evaluated in GAP and Julia, with the same results, and
# that the GAP and Julia versions of the root count do the same.
gap> evl1:= SingerAlg.ContentsOfDataFile( "splitsComb.json" )[2];;
gap> evl2:= SingerAlg.ContentsOfDataFile( "splitsOther.json" )[2];;
gap> evl2:= Filtered( evl2, l -> StartsWith( l[4], "RC(" ) );;
gap> for l in Concatenation( evl1{ [ 1 .. 500 ] }, evl2{ [ 1 .. 10 ] } ) do
>      z:= l[1];
>      qs:= List( l[2], x -> x[1] );
>      why:= l[4];
>      data_GAP:= List( qs,
>          q -> LoewyStructureInfoGAP( q, OrderMod( q, z ), z ) );
>      invs_GAP:= List( data_GAP,
>          r -> SingerAlg.InfoFromInvariantString( r, why ) );
>      if Length( Set( invs_GAP ) ) = 1 then
>        Error( "problem evaluating ", l );
>      fi;
>      if IsBound( Julia ) then
>        data_Julia:= List( qs,
>            q -> LoewyStructureInfoJulia( q, OrderMod( q, z ), z ) );
>        invs_Julia:= List( data_Julia,
>            r -> SingerAlg.InfoFromInvariantString( r, why ) );
>        if invs_GAP <> invs_Julia then
>          Error( "difference at ", l );
>        fi;
>      fi;
>    od;

##
gap> STOP_TEST( "invar.tst" );

#############################################################################
##
#E

