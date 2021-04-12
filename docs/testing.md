# Testing Approaches

**Date: 12/03/2021**

The purpose of this document is to outline the current sets of testing approaches which have been used
in the project. As of the writing of this document, testing is non-exhaustive but does cover a sizeable portion of the codebase.

All tests are written in OCaml and are designed to be executed using `dune` (or via the repo `Makefile`). All tests reside under the [tests/](../tests) directory and are primarily organized by the corresponding `src` module which they test. The exception to this is `testLiquidation.ml`, which centralizes testing logic for liquidation auctions.


So far, testing has focused on the following areas:
  1. Unit tests for key business logic
  1. Unit tests for calculation edge cases
  1. Property tests for any expected or assumed properties

Areas which remain to be tested include:

  1. Filling in cases still requiring unit / property tests. Most of the remaining test cases are spread throughout the codebase and not centralized to a single module.
  1. Addition of some higher-level end to end tests which actually call the contract. Tests do **not** currently evaluate the Tezos operations which the Checker code generates since no interpreter exists for them in Ligo at the moment.
    1. We will want these to also test the inter-contract dynamics between Checker and burrows, etc.


## Unit tests

A core part of the current test suite are unit tests which check basic application logic such as
the logic for handling tickets (which are used for permissioning) and ensuring that expected error codes are thrown. We've provided a test in these areas for nearly all of the main entrypoint code in `checker.ml` and have also added many to `burrow.ml` and `uniswap.ml`.

One interesting pattern in the unit tests is the setting of the mutable references in `Ligo.Tezos` before (and sometimes within) test logic. This pattern is used to simulate execution within the context of a transaction.

```ocaml
 Ligo.Tezos.reset ();
 Ligo.Tezos.new_transaction
    ~seconds_passed:1
    ~blocks_passed:1
    ~sender:alice_addr
    ~amount:(Ligo.tez_from_literal "0mutez");

 assert_raises
    (Failure (Ligo.string_of_int error_BuyKitNoTezGiven))
    (fun () ->
       ...
      );
```

In addition to tests for handling of permissions and errors, we have also written a number of unit tests which aim to check boundary cases for the many calculations Checker performs. These tests are primarily spread throughout `burrow.ml`, `parameters.ml`, `uniswap.ml`. In general, we have aimed to err on the side of caution by hard coding the bounds for test conditions. This makes the tests more sensitive to changes to internal calculation logic (e.g. floor vs ceil division) as well as changes to the constants defined in `constants.ml`.

## Property tests

A number of property tests exist for expected properties (i.e. from the spec) as well as properties which we assume in the current implementation.  These tests are written using `QCheck` and incorporated into the main `OUnit` test suite. One challenge in writing property tests for Checker is that many of the calculations have several inputs (especially those which rely on  `Parameters`) which makes the search space for randomized testing large. To reduce the number of samples required to detect failures, we have tried to hold some of the inputs constant where appropriate.

As with many of the unit tests, we have made hard-coded the bounds for many of the randomized parameters where applicable (e.g. when the range of valid values for inputs are dependent on eachother). As described above, this helps make the tests more sensitive to changes in complex numerical calculations.

## State management

Checker performs a lot of internal record-keeping, managing state for things like the amount of kit associated with a burrow, burrow configurations, tracking lots sent to auction, etc. We use two functions for checking that a valid state is maintained: 1. `Checker.assert_checker_invariants` and `Burrow.assert_burrow_invariants`. Currently, we have written tests for many (but not all) of the functions which return a new checker or burrow state. See [tests/testBurrow.ml](../tests/testBurrow.ml) for examples.
