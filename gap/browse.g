#############################################################################
##
#W  browse.g             GAP 4 package SingerAlg                Thomas Breuer
##
##  This file contains GAP functions that are available only if the
##  Browse package is available.
##


#############################################################################
##
#F  BrowseSingerAlgebras( <cond1>, <val1>, <cond2>, <val2>, ... )
##
##  <#GAPDoc Label="BrowseSingerAlgebras">
##  <ManSection>
##  <Func Name="BrowseSingerAlgebras" Arg="cond1, val1, cond2, val2, ..."/>
##
##  <Returns>
##  the list of data selected in visual mode.
##  </Returns>
##  <Description>
##  This function shows the precomputed data about the Singer algebras of
##  dimension up to <M>&MAXZ;</M> in a <Package>Browse</Package> table.
##  The columns of the table correspond to the parameters
##  <C>z</C>, <C>q</C>, <C>n</C>, <C>LL</C> (the Loewy length),
##  <C>m(q,e)</C>, <C>diff</C> (the difference
##  <M>\lfloor n(q-1)/m(q,e) \rfloor + 1 - LL</M>),
##  and the information whether the isomorphism type of the algebra with
##  the given parameters is classified or not
##  (an empty string or the flag <C>-</C>, respectively).
##  <P/>
##  This function is available only if the <Package>Browse</Package>
##  package is available.
##  <P/>
##  <Example><![CDATA[
##  gap> if IsBound( BrowseSingerAlgebras ) then
##  >   bsp:= [ NCurses.keys.BACKSPACE ];;  # hit the BACKSPACE key
##  >   d:= [ NCurses.keys.DOWN ];;  # hit the down arrow
##  >   l:= [ NCurses.keys.LEFT ];;  # hit the left arrow
##  >   BrowseData.SetReplay( Concatenation(
##  >         # select the 'isom?' column
##  >         "scrrrrrr",
##  >         # restrict the table to rows with unknown isom. type
##  >         "f-", [ NCurses.keys.ENTER ],
##  >         # clear the restriction
##  >         "!",
##  >         # select the 'diff' column
##  >         "scrrrrr",
##  >         # restrict the table to rows with nonzero 'diff'
##  >         "f", bsp, "0", d, d, d, d, l, [ NCurses.keys.ENTER ],
##  >         # clear the restriction
##  >         "!",
##  >         # select the first entry with 'z = 100'
##  >         "sc/100", [ NCurses.keys.ENTER ],
##  >         # add this entry to the result
##  >         [ NCurses.keys.ENTER ],
##  >         # and quit the applications
##  >         "Q" ) );
##  >   BrowseSingerAlgebras();;
##  >   BrowseData.SetReplay( false );
##  > fi;
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
BindGlobal( "BrowseSingerAlgebras", function( arg )
    local list, m, r, l, n, sel_action, t, i;

    list:= [];

    if Length( arg ) = 0 then
      # Show the contents of the whole database.
      # Delay loading the data.
      # For that, initialize the indices if necessary.
      if not IsBound( SingerAlg.LoewyVectorsDataIndex ) then
        SingerAlg.LoewyVectorsDataIndex:=
            SingerAlg.ContentsOfDataFile( "index.json" )[2];
      fi;
      m:= Length( SingerAlg.LoewyVectorsDataIndex );
    else
      # Let 'AllSingerAlgebraInfos' select the requested data.
      for r in CallFuncList( AllSingerAlgebraInfos, arg ) do
        l:= [ r.z, r.q, r.n, r.LL, r.m, r.diff, "-" ];
        if r.isom <> fail then
          l[7]:= r.isom[2];
        fi;
        Add( list, l );
      od;
      m:= Length( list );
    fi;

    n:= 7;

    sel_action:= rec(
      helplines:= [ "add the current parameters to the result list" ],
      action:= function( t )
        local i;

        if t.dynamic.selectedEntry <> [ 0, 0 ] then
          i:= t.dynamic.indexRow[ t.dynamic.selectedEntry[1] ] / 2;
          Add( t.dynamic.Return, list[i]{ [ 1, 2 ] } );
        fi;
      end );

    t:= rec(
        work:= rec(
          align:= "c",
          header:= t -> BrowseData.HeaderWithRowCounter( t,
                          "Singer Algebras",
                          m ),
          main:= [],
          Main:= function( t, i, j )
            local paras, r;

            if not IsBound( list[i] ) then
              paras:= SingerAlg.LoewyVectorsDataIndex[i];
              r:= CallFuncList( SingerAlgebraData, paras );
              list[i]:= [ r.z, r.q, r.n, r.LL, r.m, r.diff, "-" ];
              if r.isom <> fail then
                list[i][7]:= r.isom[2];
              fi;
            fi;
            return String( list[i][j] );
          end,
          m:= m,
          n:= n,
          labelsCol:= [ [ "z", "q", "n", "LL", "m(q,e)", "diff", "isom" ] ],
          sepLabelsCol:= [ "-", "-" ],
          sepCol:= [ "| ", " | ", " | ", " | ", " | ", " | ", " | ", " |" ],

          # Avoid computing all column entries for determining the width.
          widthCol:= [ , 5,, 5,, 4,, 5,, 6,, 4,, 5 ],

          SpecialGrid:= BrowseData.SpecialGridLineDraw,

          Click:= rec(
            select_column_and_entry:= sel_action,
            select_entry:= sel_action,
            select_row:= sel_action,
          ),
        ),

        dynamic:= rec(
          sortFunctionsForColumns:= ListWithIdenticalEntries( 6,
              BrowseData.CompareAsNumbersAndNonnumbers ),

          Return:= [],
        ),
      );

    # Customize the sort parameters.
    for i in [ 1 .. n ] do
      BrowseData.SetSortParameters( t, "column", 1,
          [ "add counter on categorizing", "yes" ] );
    od;

    # Show the data, return selected data.
    return NCurses.BrowseGeneric( t );
    end );


#############################################################################
##
##  Add the Browse application to the list shown by `BrowseGapData'.
##
BrowseGapDataAdd( "Singer Algebra Overview", BrowseSingerAlgebras, true, "\
an overview of the database of Singer algebras \
of dimension up to 10001, \
the return value is the list of parameter pairs [ z, q ] \
encoding the clicked entries" );


#############################################################################
##
#F  BrowseSingerMonomials( <A> )
#F  BrowseSingerMonomials( <q>, <z> )
#F  BrowseSingerMonomials( <q>, <n>, <e> )
##
##  <#GAPDoc Label="BrowseSingerMonomials">
##  <ManSection>
##  <Heading>BrowseSingerMonomials</Heading>
##  <Func Name="BrowseSingerMonomials" Arg='A'
##   Label="for a Singer algebra"/>
##  <Func Name="BrowseSingerMonomials" Arg='q, z'
##   Label="for parameters q, z"/>
##  <Func Name="BrowseSingerMonomials" Arg='q, n, e'
##   Label="for parameters q, n, e"/>
##
##  <Returns>
##  nothing.
##  </Returns>
##  <Description>
##  Let <A>A</A> be a Singer algebra <M>A[<A>q</A>, n, <A>z</A>]</M>,
##  where <M>n</M> is the multiplicative order of <A>q</A> modulo <A>z</A>,
##  or let <A>q</A> and <A>z</A> be two coprime integers that define
##  such an algebra <M>A[<A>q</A>, n, <A>z</A>]</M>,
##  or let <A>q</A>, <A>n</A>, <A>e</A> be parameters such that
##  <M><A>z</A> = (<A>q</A>^{{<A>n</A>}}-1) / <A>e</A></M> holds.
##  <P/>
##  <Ref Func="BrowseSingerMonomials" Label="for a Singer algebra"/>
##  opens a Browse table showing three column parts:
##  The first column contains the positions <M>0, 1, \ldots</M> of the Loewy
##  layers of <A>A</A>,
##  the second column contains the dimensions of the Loewy layers,
##  and the remaining columns describe the elements of the canonical basis
##  of <A>A</A> in each layer;
##  by default, the element <M>b_i</M> is represented by <M>i</M>,
##  but when the global option <C>m</C> is present (which stands for
##  <Q>monomials</Q>) then <M>B_i</M> is represented by the coefficients
##  of the <A>q</A>-adic expansion of <M>i <A>e</A></M>.
##  The first two columns are regarded as row labels, that is,
##  their horizontal position is fixed on the screen when one scrolls to the
##  left or right in the third column
##  <P/>
##  <Ref Func="BrowseSingerMonomials" Label="for a Singer algebra"/>
##  is based on the <Ref Func="NCurses.BrowseDenseList" BookName="Browse"/>
##  function from the <Package>Browse</Package> package <Cite Key="Browse"/>;
##  it is available only if this package is available.
##  <P/>
##  <Example><![CDATA[
##  gap> if IsBound( BrowseSingerMonomials ) then
##  >   n:= [ 14, 14, 14 ];;  # ``do nothing'' input
##  >   BrowseData.SetReplay( Concatenation( n, n, "Q" ) );
##  >   BrowseSingerMonomials( 2, 7 );
##  >   BrowseData.SetReplay( false );
##  > fi;
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
BindGlobal( "BrowseSingerMonomials", function( arg )
    local paras, q, n, z, data, entries, mat, i;

    if Length( arg ) = 1 and IsSingerAlgebra( arg[1] ) then
      paras:= ParametersOfSingerAlgebra( arg[1] );
      q:= paras[1];
      n:= paras[2];
      z:= paras[3];
      data:= LoewyStructureInfo( arg[1] );
    elif Length( arg ) = 2 then
      q:= arg[1];
      z:= arg[2];
      n:= OrderMod( q, z );
      data:= LoewyStructureInfo( q, n, z );
    elif Length( arg ) = 3 then
      q:= arg[1];
      n:= arg[2];
      z:= (q^n-1)/arg[3];
      data:= LoewyStructureInfo( q, n, z );
    else
      Error( "usage: BrowseSingerMonomials( <A> ) or\n",
             "BrowseSingerMonomials( <q>, <z> ) or\n",
             "BrowseSingerMonomials( <q>, <n>, <e> )" );
    fi;

    if ValueOption( "m" ) = true then
      entries:= List( data.monomials,
                  x -> Concatenation( "(",
                         JoinStringsWithSeparator( List( x, String ), "," ),
                         ")" ) );
    else
      entries:= List( [ 0 .. z ], String );
    fi;
    mat:= List( [ 1 .. data.LL ], x -> [] );
    for i in [ 1 .. Length( data.layers ) ] do
      Add( mat[ data.layers[i]+1 ], entries[i] );
    od;

    NCurses.BrowseDenseList( mat,
        rec( header:= [ Concatenation( "A[", String(q), ",", String(n), ",",
                            String(z), "]" ), "" ],
             labelsRow:= List( [ 0 .. data.LL-1 ],
                               i -> [ String( i ),
                                      Concatenation( " ",
                                          String( Length( mat[i+1] ) ) ) ] ),
             labelsCol:= [] ) );
    end );


#############################################################################
##
#E

