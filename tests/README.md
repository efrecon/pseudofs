# Test Suite

This directory contains the test suite for this module. The files are as
follows:

* [all.tcl](all.tcl) is a wrapper to run the entire test suite.
* [proc.test](proc.test) contains the unit tests targetting processes.
* [sys.test](sys.test) contains the unit tests targetting the system as a whole.

To run the suite, change to this directory and run the following command. This
will run the entire test suite, with some verbosity as to which tests are
passing (the default is to be silent on passing tests).

    ./all.tcl -verbose "bpe"