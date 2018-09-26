# ProcFS

This library implements a Tcl 8.6 interface to the (Linux) [proc] filesystem.
The library has grown out of personal needs and does not (yet?) implement all
process or system related interfaces. The design philosophy behind the
implementation is to stay as close as possible to the original pseudo
[filesystem][man], while providing a Tcl-touch to how results are mediated back
to callers.

  [proc]: https://en.wikipedia.org/wiki/Procfs
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
utility functions for reading files present within the pseudo [filesystem][man].
As most of these are formatted in more or less similar ways, interfacing missing
features is usually a matter of reading the manual and a few lines of
implementation code.