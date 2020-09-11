#############################################################################
##
#W  read.g          GAP 4 package `SingerAlg'                   Thomas Breuer
##
#Y  Copyright (C) 2019, Lehrstuhl D für Mathematik, RWTH Aachen, Germany
##

ReadPackage( "SingerAlg", "gap/loewy.gi" );
ReadPackage( "SingerAlg", "gap/mindeg.gi" );
ReadPackage( "SingerAlg", "gap/loewylen.g" );
ReadPackage( "SingerAlg", "gap/access.g" );
ReadPackage( "SingerAlg", "gap/invar.g" );
ReadPackage( "SingerAlg", "gap/permiso.g" );
ReadPackage( "SingerAlg", "gap/utils.g" );
ReadPackage( "SingerAlg", "gap/display.g" );

if IsPackageMarkedForLoading( "Browse", ">= 1.8.9" ) then
  ReadPackage( "SingerAlg", "gap/browse.g" );
fi;

