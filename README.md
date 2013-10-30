![Yarr](https://raw.github.com/brownplt/pyret-lang/master/img/pyret-banner.png)

A scripting language.

The use of vocabulary from
http://reservationsbvi.com/thisoldpirate/glossary.html is recommended
when commenting and reporting issues.


Setting Sail
------------

First, make sure ye've installed [Racket >= 5.3.4](http://racket-lang.org). Then run:

    $ make dep && make && make test

It'll build the Pyret compiler, run the tests, and hoist the
anchor.

When yer ready to brave the sea, visit [the introduction](https://github.com/brownplt/pyret-lang/blob/master/docs/introduction.markdown).


Setting up Resugaring
---------------------

First, make sure you have a recentish version of Haskell
installed. (Ideally, install the Haskell platform --
http://www.haskell.org/platform/).

Make sure the lib/Resugarer submodule is up to date. Run `make` in
lib/Resugarer/hs, and then symlink lib/Resugarer/hs/Resugarer (the
executable that was just produced) to src/lang/stepper/Resugarer.

Now you should be ready to run resugaring. Just use the '--trace' option:

    raco pyret --trace examples/ahoy-world.arr

Before the Pyret program is run, it will be prefixed by the output of
Pyret's standard resugaring ('--std-->') and its desugaring as performed
by the Resugarer system ('--new-->'). These ought to be identical!

As it runs, each evaluation step will be printed as a Pyret AST. Terms in
the AST that have been reduced to values are surrounded by angle brackets (<...>).

This is a work in progress.
