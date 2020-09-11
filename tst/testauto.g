#############################################################################
##
#W  testauto.g           GAP 4 package SingerAlg                Thomas Breuer
##
#Y  Copyright (C)  2020,   Lehrstuhl D für Mathematik,  RWTH Aachen,  Germany
##
##  Running these tests requires only a few minutes of CPU time,
##  contrary to 'tst/testall.g'.
##  Therefore, this testfile is listed in 'PackageInfo.g',
##  and 'tst/testall.g' is not.
##

LoadPackage( "SingerAlg", false );
dirs:= DirectoriesPackageLibrary( "SingerAlg", "tst" );
optrec:= rec( compareFunction:= "uptowhitespace" );

# Test the manual examples but omit the 'Browse' related ones.
success:= Test( Filename( dirs, "docxpl2.tst" ), optrec );

# Report overall test results.
if success then
  Print( "#I  No errors detected while testing\n\n" );
  GAP_EXIT_CODE( 0 );
else
  Print( "#I  Errors detected while testing\n\n" );
  GAP_EXIT_CODE( 1 );
fi;


#############################################################################
##
#E

