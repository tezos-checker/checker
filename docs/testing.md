# Testing Approaches

**Date: 2021-04-30**

The purpose of this document is to outline the current testing approaches which
have been used in the project. As of the writing of this document, testing is
non-exhaustive but does cover approximately 75% of the
codebase.

All tests are written in OCaml (OUnit and QCheck) and are designed to be
executed using `dune` (or via the repo `Makefile`) since there is not a unit
testing framework for Ligo yet. All tests reside under the [tests/](../tests)
directory and are primarily organized by the corresponding `src` module which
they test. The exceptions to this are `testLiquidation.ml`, which centralizes
testing logic for calculations related to burrow liquidation, and
`testLiquidationAuction.ml` which contains tests for the general logic of
liquidation auctions.

So far, testing has focused on the following areas:
  1. Unit tests for key business logic
  1. Unit tests for calculation edge cases
  1. Property tests for properties of the
     [spec](https://hackmd.io/teMO2x9PRRy1iTBtrSMBvA) as well as for properties
     which are assumed in the implementation. The latter are primarily
     math-related.
  1. Model checking of data structure implementations

Areas which remain to be tested include:

  1. Filling in cases still requiring unit / property tests. Most of the
     remaining test cases are spread throughout the codebase and are not
     centralized to a single module.
  1. Addition of some higher-level end-to-end tests which actually call the
     contract. Tests do **not** currently evaluate the Tezos operations which
     the Checker code generates since no interpreter exists for them in Ligo at
     the moment. Some initial work has begun on this front but has not landed
     yet.
     1. We will want these to also test the inter-contract dynamics between
        Checker and the other contracts which it relies on, namely the
        individual contracts for burrows, the price oracle, as well as
        the [ctez contract](https://github.com/murbard/ctez).

## Unit tests

A core part of the current test suite are unit tests which check basic
application logic such as handling tickets (which are used for permissioning)
and ensuring that expected error codes are thrown. We've provided tests in these
areas for nearly all of the main entrypoint code in `checker.ml` and have also
added many to `burrow.ml` and `cfmm.ml`.

The tests in `testChecker.ml` are of particular interest, since they are
somewhat higher-level functional tests that simulate calling the contract's
entrypoints in the context of a Tezos transaction. (Mutable state in our
`Ligo.Tezos` module is used to achieve this.)

In addition to tests for handling of permissions and errors, we have also
written a number of unit tests which aim to check boundary cases for the many
calculations Checker performs. These tests are primarily spread throughout
`burrow.ml`, `parameters.ml`, `cfmm.ml`. In general, we have aimed to err on
the side of caution by hard coding the bounds for test conditions. This makes
the tests more sensitive to changes to internal calculation logic (e.g. floor vs
ceil division) as well as changes to the constants defined in `constants.ml`.

## Property tests

A number of property tests exist for expected properties (i.e. from the
[spec](https://hackmd.io/teMO2x9PRRy1iTBtrSMBvA)) as well as properties which we
assume in the current implementation.  These tests are written using `QCheck`
and incorporated into the main `OUnit` test suite.

One challenge in writing property tests for Checker is that many of the
calculations have several inputs (especially those which rely on `Parameters`)
which makes the search space for randomized testing large. To reduce the number
of samples required to detect failures, we have tried to hold some of the inputs
constant where appropriate.

Another challenge in writing tests for Checker is that portions of the codebase
make assumptions about user behavior. For example, the calculation of the
imbalance index assumes that Checker would not remain untouched for 20 years.
These types of assumptions force us to include additional constraints on the
bounds of randomized test inputs to ensure that they match our expectations of user
behavior and add additional cases to check for failure when these expectations
are not met.

As with many of the unit tests, we have hard-coded the bounds for many of the
randomized parameters where applicable (e.g. when the range of valid values for
inputs are dependent on eachother). As described above, this helps make the
tests more sensitive to changes in complex numerical calculations.

## Model checking

Checker implements a double ended queue which it uses for its liquidation
auction logic. As an additional check on our implementation, we compare it to a
model implementation from the `core_kernel` library. The comparison is performed
by applying a random series of operations on both our and the reference
implementation using `QCheck` and comparing the two queues at each iteration.
This test resides in [testAvlModel.ml](../tests/testAvlModel.ml).

## State management

Checker performs a lot of internal record-keeping, managing state for things
like the amount of kit associated with a burrow, burrow configurations, tracking
lots sent to auction, etc. We use several functions for checking that a valid
state is maintained:

  1. `Checker.assert_checker_invariants`
  1. `LiquidationAuction.assert_liquidation_auction_invariants`
  1. `Avl.assert_avl_invariants`

Currently, we have written tests for many (but not all) of the functions which
return a new checker or burrow state. See
[tests/testBurrow.ml](../tests/testBurrow.ml) for examples.

## Code coverage

The project makefile is configured to generate code coverage reports which we
use to help guide ongoing testing efforts. Code coverage is run in OCaml as a
part of our test suite. When interpreting coverage reports, it is important to
note that sections of the codebase are written for compatibility with OCaml /
testing purposes and are removed when transpiling to Ligo (Denoted by a `(*
BEGIN_OCAML *) ... (* END_OCAML *)`)
