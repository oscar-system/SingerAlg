#############################################################################
##
#W  testall.g            GAP 4 package SingerAlg                Thomas Breuer
##
#Y  Copyright (C)  2020,   Lehrstuhl D für Mathematik,  RWTH Aachen,  Germany
##

LoadPackage( "SingerAlg", false );
dirs:= DirectoriesPackageLibrary( "SingerAlg", "tst" );
optrec:= rec( compareFunction:= "uptowhitespace" );

# Test the manual examples but omit the 'Browse' related ones.
success:= Test( Filename( dirs, "docxpl2.tst" ), optrec );

success:= Test( Filename( dirs, "mindeg.tst" ), optrec ) and success;

success:= Test( Filename( dirs, "loewylen.tst" ), optrec ) and success;

success:= Test( Filename( dirs, "loewystr.tst" ), optrec ) and success;

success:= Test( Filename( dirs, "database.tst" ), optrec ) and success;

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

