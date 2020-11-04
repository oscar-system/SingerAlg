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
gap> evl2a:= Filtered( evl2, l -> StartsWith( l[4], "RC(" ) );;
gap> evl2b:= Filtered( evl2, l -> l[4] = "LL4QuoDerDim" );;
gap> z:= "dummy";;  why:= "dummy";;  # Avoid syntax warnings ...
gap> for l in Concatenation( evl1{ [ 1 .. 500 ] }, evl2a{ [ 1 .. 10 ] },
>                            evl2b{ [ 1 .. 10 ] } ) do
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

# Check that the implementation of 'SingerAlg.RightDerivationsDimension'
# yields the same results as the naive approach.
gap> for z in [ 2 .. 30 ] do
>      for q in PrimeResidues( z ) do
>        if q = 1 then
>          q:= z+1;
>        fi;
>        a:= SingerAlgebra( q, z, GF(2) );
>        der:= Derivations( CanonicalBasis( a ) );
>        dim:= Dimension( der );
>        data:= LoewyStructureInfoGAP( a );
>        T:= SingerAlg.MultTable( data );
>        if SingerAlg.RightDerivationsDimension( T ) <> dim then
>          Error( "difference in dimension of derivations" );
>        fi;
>        if IsBound( Julia ) then
>          data:= LoewyStructureInfoJulia( a );
>          T:= Julia.SingerAlg.MultTable( data );
>          if Julia.SingerAlg.RightDerivationsDimension( T ) <> dim then
>            Error( "difference GAP/Julia in dimension of derivations" );
>          fi;
>        fi;
>      od;
>    od;

# Check that the implementations of 'SingerAlg.LL4QuoDerDim' and
# 'Julia.SingerAlg.LL4QuoDerDim' yield the same results as
# the naive GAP approach.
gap> SingerAlg.LL4QuoDerDimNaive:= function( data, maxdim... )
>     local q, z, n, T, V, A, basis, radser, u;
> 
>     if data.LL <> 4 then
>       return fail;
>     elif Length( maxdim ) = 0 or not IsPosInt( maxdim[1] ) then
>       maxdim:= 50;
>     else
>       maxdim:= maxdim[1];
>     fi;
> 
>     q:= data.parameters[1];
>     z:= data.parameters[3];
>     n:= z+1;
>     T:= SingerAlg.MultTable( data );;
>     V:= Difference( [ 2 .. n ],
>             Filtered( [ 1 .. n ],
>                 i -> ForAll( Difference( [ 2 .. n ], [ n+1-i ] ),
>                          x -> T[i,x] = 0 ) ) );;
>     if Length( V ) > maxdim then
>       return fail;
>     fi;
>     A:= SingerAlgebra( q, z, GF(2) );;
>     basis:= CanonicalBasis( A );;
>     radser:= SingerAlg.BasesOfRadicalSeries( data );
>     u:= SubalgebraNC( A, basis{ Union( V, radser[2] ) }, "basis" ) /
>             SubalgebraNC( A, basis{ [ z+1 ] }, "basis" );
>     return Dimension( Derivations( Basis( u ) ) );
>    end;;
gap> for z in [ 2 .. 40 ] do
>      for q in PrimeResidues( z ) do
>        data:= LoewyStructureInfoGAP( q, z );
>        res:= SingerAlg.LL4QuoDerDimNaive( data, z );
>        if res <> SingerAlg.LL4QuoDerDim( data, z ) then
>          Error( "difference in LL4QuoDerDimNaive vs. LL4QuoDerDim" );
>        fi;
>        if IsBound( Julia ) then
>          data:= LoewyStructureInfoJulia( q, z );
>          if res <> Julia.SingerAlg.LL4QuoDerDim( data, z ) then
>            Error( "difference GAP/Julia in LL4QuoDerDim" );
>          fi;
>        fi;
>      od;
>    od;

##
gap> STOP_TEST( "invar.tst" );

#############################################################################
##
#E

