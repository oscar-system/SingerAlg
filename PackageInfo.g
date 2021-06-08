#############################################################################
##
##  PackageInfo.g for the GAP 4 package SingerAlg               Thomas Breuer
##
SetPackageInfo( rec(
PackageName :=
  "SingerAlg",
Version :=
  "1.0.2",
MyWWWHome :=
  "https://www.math.rwth-aachen.de/~Thomas.Breuer",
Subtitle :=
  "Loewy lengths of certain algebras",
Date :=
  # "11/09/2020", # Version 1.0.0
  # "13/01/2021", # Version 1.0.1
  "08/04/2021",   # Version 1.0.2
License :=
  "GPL-3.0-or-later",
SourceRepository := rec(
  Type:= "git",
  URL:= "https://github.com/oscar-system/SingerAlg",
  ),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome :=
  Concatenation( ~.MyWWWHome, "/", LowercaseString( ~.PackageName ) ),
ArchiveURL :=
  Concatenation( ~.PackageWWWHome, "/", LowercaseString( ~.PackageName ),
                 "-", ~.Version ),
ArchiveFormats :=
  ".tar.gz",
Persons := [
  rec(
    LastName := "Breuer",
    FirstNames := "Thomas",
    IsAuthor := true,
    IsMaintainer := true,
    Email := "sam@math.rwth-aachen.de",
    WWWHome := ~.MyWWWHome,
    Place := "Aachen",
    Institution := "Lehrstuhl für Algebra und Zahlentheorie, RWTH Aachen",
    PostalAddress := Concatenation( [
      "Thomas Breuer\n",
      "Lehrstuhl für Algebra und Zahlentheorie\n",
      "Pontdriesch 14/16\n",
      "52062 Aachen\n",
      "Germany"
      ] ),
    ),
  ],
Status :=
  "deposited",
README_URL :=
  Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL :=
  Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
AbstractHTML := Concatenation( [
  "The package provides functionality for studying the Loewy structure ",
  "of certain algebras, ",
  "and provides a database."
  ] ),
PackageDoc := [
  rec(
    BookName :=
      "SingerAlg",
    ArchiveURLSubset :=
      [ "doc" ],
    HTMLStart :=
      "doc/chap0.html",
    PDFFile :=
      "doc/manual.pdf",
    SixFile :=
      "doc/manual.six",
    LongTitle :=
      "Loewy structure of certain algebras",
  ) ],
Dependencies := rec(
  GAP :=
    ">= 4.11.0",                  # because of a few library functions
  OtherPackagesLoadedInAdvance := [
    ],
  NeededOtherPackages := [
      [ "gapdoc", ">= 1.6.2" ],   # for the documentation
    ],
  SuggestedOtherPackages := [
      [ "JuliaInterface", ">= 0.4.3" ],  # Julia based speedup
      [ "Browse", ">= 1.8.9" ],   # because of database attributes
                                  # and overview functions; need the fix of
                                  # 'NCurses.BrowseDenseList' from 1.8.9
      [ "GraPe", ">= 4.8" ],      # because of permutation isomorphisms
      [ "FactInt", ">= 1.6.3" ],  # because 'Factors' is called a lot
    ],
  ExternalConditions := [
    ],
  ),
AvailabilityTest :=
  ReturnTrue,
TestFile :=
  "tst/testauto.g",
Keywords :=
  [ "Loewy length", "Loewy vector", "GAP-Julia integration" ],
) );


#############################################################################
##
#E

