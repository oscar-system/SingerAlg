The GAP 4 Package **SingerAlg**
===============================

Description
-----------

The aims of the GAP package **SingerAlg** are
to document the computations needed in the papers
[The Loewy structure of certain fixpoint algebras, Part I](https://doi.org/10.1016/j.jalgebra.2019.05.004)
(see also [the MR report](http://www.ams.org/mathscinet-getitem?mr=4102117))
and
[The Loewy structure of certain fixpoint algebras, Part II](http://arxiv.org/abs/1912.03065v1),
and to give access to the related GAP/Julia functions and to the database of
Singer algebras of dimension up to 10000.

For further information see [the homepage of the package](http://www.math.rwth-aachen.de/~Thomas.Breuer/singeralg/),
where package archives can be found as well as
the [data files of low dimensional Singer algebras](http://www.math.rwth-aachen.de/~Thomas.Breuer/singeralg/data/).


Authors
-------

The code was written by [Thomas Breuer](http://www.math.rwth-aachen.de/~Thomas.Breuer/).
The underlying theory was developed in the abovementioned papers,
by Thomas Breuer, László Héthelyi, Erzsébet Horváth, and Burkhard Külshammer.

For questions or suggestions write to

    Thomas.Breuer@Math.RWTH-Aachen.De

or use [the issue tracker of the git repository](https://github.com/oscar-system/SingerAlg/issues).


Installation
------------

In principle it is enough to unpack the archive in question in the `pkg`
subdirectory of your GAP installation.
This generates a subdirectory `SingerAlg-x.y.z`.

No kernel module has to be compiled,
but the package provides also functions that rely on
the GAP-Julia integration via the GAP package **JuliaInterface**,
which is part of
[the Julia package GAP.jl](https://github.com/oscar-system/GAP.jl).
In particular GAP must be compiled with Julia's garbage collector
if one wants to load this package.

If you want to use the Julia functionality but you do not have
a GAP installation with GAP-Julia integration
then the easiest way to install this is
to install first Julia (at least version 1.3) and then let Julia install GAP.jl,
as described in [its README.md file](https://github.com/oscar-system/GAP.jl/blob/master/README.md).


License
-------

This package may be distributed under the terms and conditions of the
[**GNU Public License**](http://www.gnu.org/licenses) Version 3 or later.


Acknowledgement
---------------

Thomas Breuer gratefully acknowledges support by
the German Research Foundation (DFG) -- Project-ID 286237555 -- within the
[SFB-TRR 195 *Symbolic Tools in Mathematics and their Applications*](https://www.computeralgebra.de/sfb/).

