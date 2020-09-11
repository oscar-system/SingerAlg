#############################################################################
##
#W  init.g          GAP 4 package `SingerAlg'                   Thomas Breuer
##
#Y  Copyright (C) 2019, Lehrstuhl D für Mathematik, RWTH Aachen, Germany
##

# Notify the Julia part.
if IsPackageMarkedForLoading( "JuliaInterface", "" ) then
  JuliaIncludeFile(
      Filename( DirectoriesPackageLibrary( "SingerAlg", "julia" ),
                "SingerAlg.jl" ) );
# ImportJuliaModuleIntoGAP( "SingerAlg" );
#T is this needed/useful?
fi;

ReadPackage( "SingerAlg", "gap/loewy.gd");
ReadPackage( "SingerAlg", "gap/mindeg.gd" );

