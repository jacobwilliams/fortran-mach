Modern Fortran Machine Constants Module (`r1mach`, `d1mach`, `i1mach`).

### Status

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/fortran-mach.svg)](https://github.com/jacobwilliams/fortran-mach/releases/latest)
[![Build Status](https://github.com/jacobwilliams/fortran-mach/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/fortran-mach/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/fortran-mach/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobwilliams/fortran-mach)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/fortran-mach)](https://github.com/jacobwilliams/fortran-mach/commits/master)

### Compiling

A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file is included, so that the library and test cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

To use `fortran-mach` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
fortran-mach = { git="https://github.com/jacobwilliams/fortran-mach.git" }
```

Or, to use a specific version:

```toml
[dependencies]
fortran-mach = { git="https://github.com/jacobwilliams/fortran-mach.git", tag = "1.0.0" }
```

### Documentation

The latest API documentation can be found [here](https://jacobwilliams.github.io/fortran-mach/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### References

 * Bo Einarsson, [d1mach revisited: no more uncommenting DATA statements](https://wg25.taa.univie.ac.at/ifip/kyoto/workshop-info/proceedings/einarsson/d1mach.html) Presented at the [IFIP WG 2.5](https://wg25.taa.univie.ac.at) International Workshop on "Current Directions in Numerical Software and High Performance Computing", 19 - 20 October 1995, Kyoto, Japan.

