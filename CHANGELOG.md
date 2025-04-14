# Revision history for ghc-prof

## v1.4.1.13 - 2025-04-14

* Bump base upper bound so we can compile under GHC 9.6.3 ([#24](https://github.com/maoe/ghc-prof/pull/24))

## v1.4.1.12 - 2022-12-06

* Bump base to compile with GHC 9.4 ([#23](https://github.com/maoe/ghc-prof/pull/23))

## v1.4.1.11 - 2022-05-18

* Add ghcprofview-hs to the list of applications that use ghc-prof ([#21](https://github.com/maoe/ghc-prof/pull/21))
* Allow building with GHC 9.2.2 ([#22](https://github.com/maoe/ghc-prof/pull/22))

## v1.4.1.10 - 2021-10-27

* Actually allow attoparsec-0.14 by removing the upper version bound in test:regression

## v1.4.1.9 - 2021-07-16

* Allow attoparsec-0.14 (#20)

## v1.4.1.8 - 2021-02-23

* Relax upper version bounds for base and tasty
* Switch from Travis CI to GitHub Actions

## v1.4.1.7 - 2020-04-04

* Relax upper version bound for base to support GHC 8.10.1

## v1.4.1.6 - 2019-09-09

* Relax upper version bound for base
* Test with newer versions of GHC

## v1.4.1.5 - 2018-12-20

* Relax upper version bound for tasty
* Test with newer versions of GHC

## v1.4.1.4 - 2018-09-27

* Relax upper version bound for base

## v1.4.1.3 - 2018-07-09

* Relax upper version bound for containers

## v1.4.1.2 - 2018-05-15

* Relax upper version bound for tasty

## v1.4.1.1 - 2018-03-17

* Relax upper version bound for base

## v1.4.1 - 2018-02-08

* Relax upper version bound for tasty
* Add a strict decoder (#13)

## v1.4.0.4 - 2017-12-12

* Relax upper version bound for tasty-hunit

## v1.4.0.3 - 2017-11-07

* Relax upper version bound for tasty

## v1.4.0.2 - 2017-07-31

* Relax upper version bound for base

## v1.4.0.1 - 2017-04-02

* Implement a workaround for a bug in the GHC profiler ([#6](https://github.com/maoe/ghc-prof/issues/6))

## v1.4.0 - 2017-02-14

* Rename AggregateCostCentre to AggregatedCostCentre
