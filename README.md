# UCA - Unicode Components for Ada

This is an Ada 2012 library that implements [Unicode v12.1.0](https://www.unicode.org/versions/latest).

UCA is split up into components that can be linked only if required, the base library provides support for definite
string types, everything else is built on top of this. The libraries try to match the Ada standard for the other string
types.

# Building

## GNAT

```bash
$ cd build/gnat
$ make
```

# Dependencies

Ada 2012 compiler.

## Tested with

FSF GNAT 9.2.0

## [Current version](http://www.semver.org)

v0.0.10

# Copyright

Copyright (C) 2020, Luke A. Guest

# Licence

MPL 2.0
