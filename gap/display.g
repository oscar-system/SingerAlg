#############################################################################
##
#W  display.g            GAP 4 package SingerAlg                Thomas Breuer
##


#############################################################################
##
#F  SingerAlg.Pager( <string> )
##
##  Simply calling 'Pager' is not good enough, because GAP introduces
##  line breaks in too long lines, and GAP does not compute the printable
##  length of the line but the length as a string.
##
##  If <string> is empty then the builtin pager runs into an error,
##  therefore we catch this case.
##
SingerAlg.Pager:= function( string )
    if string <> "" then
      Pager( rec( lines:= string, formatted:= true ) );
    fi;
    end;


#############################################################################
##
#U  UserPreference( "SingerAlg", "DisplayFunction" )
##
##  <#GAPDoc Label="DisplayFunction">
##  <Subsection Label="subsect:displayfunction">
##  <Heading>Changing the display format: User preference <C>DisplayFunction</C></Heading>
##
##  <Index Subkey="DisplayFunction">user preference</Index>
##  The way how the function
##  <Ref Func="DisplaySingerMonomials" Label="for a Singer algebra"/>
##  shows tabular information can be customized via the user preference
##  <C>"DisplayFunction"</C> of the <Package>SingerAlg</Package> package.
##  The value must be a string that evaluates to a &GAP; function.
##  Useful values are <C>"Print"</C>
##  (see <Ref Func="Print" BookName="ref"/>),
##  <C>"PrintFormattedString"</C>
##  (see <Ref Func="PrintFormattedString" BookName="gapdoc"/>
##  in <Cite Key="GAPDoc"/>),
##  and <C>"SingerAlg.Pager"</C>,
##  which means that <Ref Func="Pager" BookName="ref"/> is called with
##  the <C>formatted</C> option,
##  which is necessary for switching off &GAP;'s automatic line breaking.
##  The default value is <C>"SingerAlg.Pager"</C> if
##  <C>GAPInfo.TermEncoding</C> has the value <C>"UTF-8"</C>,
##  and <C>"Print"</C> otherwise.
##  </Subsection>
##  <#/GAPDoc>
##
DeclareUserPreference( rec(
  name:= "DisplayFunction",
  description:= [
    "This preference controls the way how 'DisplaySingerMonomials' \
shows the requested overview. \
The value must be a string that evaluates to a GAP function. \
Useful values are \"Print\", \"PrintFormattedString\", \
and \"SingerAlg.Pager\"; \
the latter calls 'Pager' with the 'formatted' option, \
which is necessary for switching off GAP's automatic line breaking. \
The default value is \"SingerAlg.Pager\" if GAPInfo.TermEncoding \
has the value \"UTF-8\", and \"Print\" otherwise."
    ],
  default:= function()
    if GAPInfo.TermEncoding = "UTF-8" then
      return "SingerAlg.Pager";
    else
      return "Print";
    fi;
  end,
  package:= "SingerAlg",
  ) );


#############################################################################
##
#F  SingerAlg.ShowOnlyASCII()
##
##  Show nicer grids and symbols such as ℤ if the terminal admits this
##  and if the user wants it.
##  (For the package manual, we want only ASCII characters because &GAPDoc;
##  does not support special characters in its &LaTeX; and HTML versions.)
##
SingerAlg.ShowOnlyASCII:= function()
    return UserPreference( "SingerAlg", "DisplayFunction" ) = "Print" or
           GAPInfo.TermEncoding <> "UTF-8";
    end;


#############################################################################
##
#F  DisplaySingerAlgebras( <cond1>, <val1>, <cond2>, <val2>, ... )
##
##  <#GAPDoc Label="DisplaySingerAlgebras">
##  <ManSection>
##  <Func Name="DisplaySingerAlgebras" Arg="cond1, val1, cond2, val2, ..."/>
##
##  <Returns>
##  nothing.
##  </Returns>
##  <Description>
##  This function shows the precomputed data about the Singer algebras of
##  dimension up to <M>&MAXZ;</M> in a table on the screen.
##  <P/>
##  The rows of the table are determined by the arguments,
##  which have the same meaning as in the function
##  <Ref Func="AllSingerAlgebraInfos"/>.
##  (It is advisable to restrict the contents of the database to a small
##  number of rows.)
##  <P/>
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
##  gap> DisplaySingerAlgebras( "z", 7 );
##      z |     q |    n |    LL | m(q,e) | diff |  isom
##  ------+-------+------+-------+--------+------+------
##      7 |     2 |    3 |     4 |      1 |    0 |     2
##      7 |     3 |    6 |     3 |      6 |    0 |     3
##      7 |     6 |    2 |     3 |      5 |    0 |     3
##      7 |     8 |    1 |     8 |      1 |    0 |     8
##  
##  gap> DisplaySingerAlgebras( "z", [ 1 .. 200 ], "diff", IsPosInt );
##      z |     q |    n |    LL | m(q,e) | diff |  isom
##  ------+-------+------+-------+--------+------+------
##     70 |     3 |   12 |     3 |      8 |    1 |     3
##     91 |     5 |   12 |     3 |     16 |    1 |     5
##     95 |     8 |   12 |     3 |     28 |    1 |     2
##    104 |     7 |   12 |     3 |     24 |    1 |     7
##    123 |     2 |   20 |     3 |      6 |    1 |     2
##    143 |    32 |   12 |     3 |    124 |    1 |     2
##    148 |    23 |   12 |     3 |     88 |    1 |     3
##    155 |    37 |   12 |     3 |    144 |    1 |     3
##    182 |     5 |   12 |     3 |     16 |    1 |     5
##    182 |    41 |   12 |     3 |    160 |    1 |     5
##    182 |    45 |   12 |     3 |    176 |    1 |     5
##    185 |    14 |   12 |     3 |     52 |    1 |     2
##    185 |    27 |   12 |     3 |    104 |    1 |     2
##    190 |    27 |   12 |     3 |    104 |    1 |     3
##    195 |     7 |   12 |     4 |     18 |    1 |     7
##  
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
BindGlobal( "DisplaySingerAlgebras", function( arg )
    local str, l, labelsCol, n, widthCol, vert, horz1, horz2, mat, r, fun;

    # Provide columns for z, q, n, LL, m, diff, and knowledge about the Id.
    labelsCol:= [ "z", "q", "n", "LL", "m(q,e)", "diff", "isom" ];
    n:= Length( labelsCol );

    # Prescribe the column widths.
    widthCol:= [ 5, 5, 4, 5, 6, 4, 5 ];

    # Prescribe the column separators.
    if SingerAlg.ShowOnlyASCII() then
      vert:= " | ";
      horz1:= "-";
      horz2:= "-+-";
    else
      vert:= " │ ";
      horz1:= "─";
      horz2:= "─┼─";
    fi;

    mat:= [ JoinStringsWithSeparator(
                List( [ 1 .. n ],
                      i -> String( labelsCol[i], widthCol[i] ) ), vert ),
            JoinStringsWithSeparator(
                List( widthCol,
                      x -> RepeatedUTF8String( horz1, x ) ), horz2 ) ];

    for r in CallFuncList( AllSingerAlgebraInfos, arg ) do
      l:= [ r.z, r.q, r.n, r.LL, r.m, r.diff, "-" ];
      if r.isom <> fail then
        l[7]:= r.isom[2];
      fi;
      Add( mat, JoinStringsWithSeparator(
                    List( [ 1 .. n ],
                          i -> String( l[i], widthCol[i] ) ), vert ) );
    od;
    Add( mat, "\n" );

    # Show the data.
    fun:= EvalString( UserPreference( "SingerAlg", "DisplayFunction" ) );
    fun( JoinStringsWithSeparator( mat, "\n" ) );
    end );


#############################################################################
##
#F  DisplaySingerMonomials( <A> )
#F  DisplaySingerMonomials( <q>, <z> )
#F  DisplaySingerMonomials( <q>, <n>, <e> )
##
##  <#GAPDoc Label="DisplaySingerMonomials">
##  <ManSection>
##  <Heading>DisplaySingerMonomials</Heading>
##  <Func Name="DisplaySingerMonomials" Arg='A'
##   Label="for a Singer algebra"/>
##  <Func Name="DisplaySingerMonomials" Arg='q, z'
##   Label="for parameters q, z"/>
##  <Func Name="DisplaySingerMonomials" Arg='q, n, e'
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
##  <Ref Func="DisplaySingerMonomials" Label="for a Singer algebra"/>
##  prints a table showing three column parts:
##  The first column contains the positions <M>0, 1, \ldots</M> of the Loewy
##  layers of <A>A</A>,
##  the second column contains the dimensions of the Loewy layers,
##  and the remaining columns describe the elements of the canonical basis
##  of <A>A</A> in each layer;
##  by default, the element <M>b_i</M> is represented by <M>i</M>,
##  but when the global option <C>m</C> is present (which stands for
##  <Q>monomials</Q>) then <M>B_i</M> is represented by the coefficients
##  of the <A>q</A>-adic expansion of <M>i <A>e</A></M>.
##  <P/>
##  <Example><![CDATA[
##  gap> DisplaySingerMonomials( 2, 7 );  # e = 1
##  A[2,3,7]
##  
##  0 | 1 | 0
##  1 | 3 | 1 2 4
##  2 | 3 | 3 5 6
##  3 | 1 | 7
##  gap> DisplaySingerMonomials( 2, 7 : m );  # same, show monomials
##  A[2,3,7]
##  
##  0 | 1 | (0,0,0)
##  1 | 3 | (0,0,1) (0,1,0) (1,0,0)
##  2 | 3 | (0,1,1) (1,0,1) (1,1,0)
##  3 | 1 | (1,1,1)
##  gap> DisplaySingerMonomials( 9, 8 : m );  # uniserial case
##  A[9,1,8]
##  
##  0 | 1 | (0)
##  1 | 1 | (1)
##  2 | 1 | (2)
##  3 | 1 | (3)
##  4 | 1 | (4)
##  5 | 1 | (5)
##  6 | 1 | (6)
##  7 | 1 | (7)
##  8 | 1 | (8)
##  gap> DisplaySingerMonomials( 6, 7 : m );  # Loewy length 3
##  A[6,2,7]
##  
##  0 | 1 | (0,0)
##  1 | 6 | (0,5) (1,4) (2,3) (3,2) (4,1) (5,0)
##  2 | 1 | (5,5)
##  gap> DisplaySingerMonomials( 14, 15 : m );  # wrapped output
##  A[14,2,15]
##  
##  0 |  1 | ( 0, 0)
##  1 | 14 | ( 0,13) ( 1,12) ( 2,11) ( 3,10) ( 4, 9) ( 5, 8) ( 6, 7)
##    |    | ( 7, 6) ( 8, 5) ( 9, 4) (10, 3) (11, 2) (12, 1) (13, 0)
##  2 |  1 | (13,13)
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
BindGlobal( "DisplaySingerMonomials", function( arg )
    local paras, q, n, z, data, len, monomials, layers, entries, mat, i,
          width, lv, width1, width2, mid, prefix, prefixlen, string, str,
          entry, entrylen, fun;

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
      Error( "usage: DisplaySingerMonomials( <A> ) or\n",
             "DisplaySingerMonomials( <q>, <z> ) or\n",
             "DisplaySingerMonomials( <q>, <n>, <e> )" );
    fi;

    if ValueOption( "m" ) = true then

      # We want to show the monomials not the reversed ones,
      # and we want to show them in lexicographical order;
      # for that, we reverse each monomial and then sort the list of
      # monomials parallel to the corresponding 'layers' list.

      len:= Length( String( q-1 ) );
      monomials:= List( data.monomials, Reversed );
      layers:= ShallowCopy( data.layers );
      SortParallel( monomials, layers );
      entries:= List( monomials,
                  m -> Concatenation( "(",
                         JoinStringsWithSeparator(
                             List( m, i -> String( i, len ) ), "," ),
                         ")" ) );
    else
      len:= Length( String( z ) );
      entries:= List( [ 0 .. z ], i -> String( i,len ) );
      layers:= data.layers;
    fi;

    mat:= List( [ 1 .. data.LL ], x -> [] );
    for i in [ 1 .. Length( layers ) ] do
      Add( mat[ layers[i]+1 ], entries[i] );
    od;

    # Format the rows.
    width:= SizeScreen()[1] - 2;
    lv:= List( mat, row -> String( Length( row ) ) );
    width1:= Length( String( data.LL-1 ) );
    width2:= Maximum( List( lv, Length ) );

    # Show a header.
    string:= Concatenation( "A[", String(q), ",", String(n), ",", String(z),
                            "]\n\n" );

    # Show the rows.
    if SingerAlg.ShowOnlyASCII() then
      mid:= " |";
    else
      mid:= " │";
    fi;
    prefix:= Concatenation( RepeatedString( " ", width1 ), mid, " ",
                            RepeatedString( " ", width2 ), mid );
    prefixlen:= WidthUTF8String( prefix );
    for i in [ 1 .. data.LL ] do
      str:= Concatenation( String( i-1, width1 ), mid, " ",
                           String( lv[i], width2 ), mid );
      len:= WidthUTF8String( str );
      for entry in mat[i] do
        entrylen:= Length( entry ) + 1;
        if len + entrylen >= width then
          Append( str, "\n" );
          Append( string, str );
          str:= ShallowCopy( prefix );
          len:= prefixlen;
        fi;
        Append( str, " " );
        Append( str, entry );
        len:= len + entrylen;
      od;
      Append( str, "\n" );
      Append( string, str );
    od;

    fun:= EvalString( UserPreference( "SingerAlg", "DisplayFunction" ) );
    fun( string );
    end );


#############################################################################
##
#E

