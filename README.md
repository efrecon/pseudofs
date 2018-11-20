# PseudoFS

This library implements a Tcl 8.6 interface to the (Linux) [proc] and
[sys] pseudo filesystems. The library has grown out of personal needs and does
not (yet?) implement all process or system related interfaces. The design
philosophy behind the implementation is to stay as close as possible to the
original pseudo [filesystem][man], while providing a Tcl-touch to how results
are mediated back to callers.

  [proc]: https://en.wikipedia.org/wiki/Procfs
  [sys]: https://en.wikipedia.org/wiki/Sysfs
  [man]: http://man7.org/linux/man-pages/man5/proc.5.html

The implementation uses an ensemble, making it possible to call the interface
in more modern ways (no `::`). For example, the following call would return a
list with the entire command line used to start the process that performs the
call:

    procfs cmdline [pid]

Documentation for each implemented interface is (so far) provided in the code
and this ought to improve as the interface matures. This library comes with a
test [suite](tests/).

PR are welcome to improve the interface and they should arrange to provide
additional tests for the new features. The implementation provides a number of
[lambda]-based utility functions for reading files present within the pseudo
[filesystem][man]. As most of these are formatted in more or less similar ways,
interfacing missing features is usually a matter of reading the manual and a few
lines of implementation code.

  [lambda]: https://www.tcl.tk/man/tcl/TclCmd/apply.htm

## Code Organisation

Code is orgarnised as follows:

* The [pseudofs](./pseudofs-0.1.tm) module contains common code for generic
  lambda-based file reading and general module configuration.
* The [procfs](./procfs-0.2.tm) module contains an interface to the procfs
  pseudo file system. It is the most developed implementation as it contains
  what formed the initial core of the implementation.
* The [sysfs](./sysfs-0.1.tm) module is meant to provide an interface to the
  [sys] pseudo filesystem and, thusfar, only contains a reimplementation of the
  a procedure inspired by [lsblk] and taking the same column names as arguments
  (not all implemented).

Note and **WARNING**: In order to provide a better programming interface to
outer callers, the API of [pseudofs](./pseudofs-0.1.tm) creates a number of
commands which names are identical to *VERY* common Tcl commands. From the
outside, this is an ensemble and provide a good API. From the inside, this means
that programmers should pay special attention to calling the Tcl commands in
their fully-qualified form, i.e. `::file` instead of `file`.

  [lsblk]: http://man7.org/linux/man-pages/man8/lsblk.8.html