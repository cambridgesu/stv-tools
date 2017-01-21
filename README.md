STV Tools
=========

This is a set of tools for automated counting of STV elections. At present,
it reads in BLT files and validates them. Ultimately it will implement
the ERS97 dialect of the Single Transferable Vote voting system.

It is intended that there will be at least three implementations of
STV in this project:

* a reference implementation, designed to be simple and easier to
  verify as being correct; this is written in idiomatic OCaml, without
  the use of variables, mutable data structures, and with clearly
  documented and enforced invariants

* two conventional-language implementations, structured analogously to
  the reference implementation and intended to be bug-for-bug compatible
  with it, in necessarily slightly unidiomatic Python and
  Java, for inspection by a broader community of users

There will also be a test suite, particularly of BLT files.

It is assumed that BLT input files may be inadvertently or
deliberately malformed, and that this should be detected rather than
triggering unexpected or exploitable behaviour directed against the election
being counted or against the machine being used to do so.


Links
=====

* BLT files (link todo)

Installation
============

These instructions assume a pre-installed OCaml system with various
build-time dependencies already available; instructions for establishing
this will be provided later.

Having checked out this repository, do:

    $ (cd reference && oasis setup && ocaml setup.ml -configure && ocaml setup.ml -build)
    $ reference/test_stv.byte --dir test_data
    $ reference/main.byte < test_data/example.blt


Coding Style
============

The reference implementation avoids all advanced features of the OCaml
language, but does employ some terse abbreviated syntax where
idiomatic.  Programmers with three or four months' OCaml experience
should have no trouble following the code.