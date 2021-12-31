#!/usr/bin/env python
"""
Rudimentary mutation tests for a pre-defined set of OCaml source modules
and mutations.

See '--help' for usage, an example:

./scripts/mutate.py --test 'dune build @run-fast-tests' --num-mutations 2 parameters.ml

This script is currently meant to be used in a more interactive fashion. Edit the
mutations MUTATION_GROUPS as needed.

You can use the (* SKIP_MUTATION *) in-line comment to have this script ignore
certain lines in the source modules.

WARNING: This script edits the src files in-place and attempts to restore changes
using git. If the script exits ungracefully you might need to clean up your
working tree.
"""

import os
import random
import re
import subprocess
import argparse
from enum import Enum, auto
from contextlib import contextmanager
from pprint import pprint
from typing import Optional, Tuple
from typing import Callable

# Custom type aliases can go here
MutationFormatter = Callable[[str], str]


class MutationType(Enum):
    # Swap out a function for another one with the same type
    SWAP_FUNCTION = auto()
    # Update an integer literal value
    INTEGER_LITERAL = auto()


# Each group contains functions with the same type signature which can be swapped
# and still allow the program to compile.
MUTATION_GROUPS = [
    ## MATH
    # KIT
    {"kit_zero", "kit_one"},
    {"kit_add", "kit_sub", "kit_min", "kit_max"},
    {"geq_kit_kit", "lt_kit_kit", "gt_kit_kit", "eq_kit_kit"},
    {"kit_of_fraction_ceil", "kit_of_fraction_floor"},
    # CTEZ
    {"ctez_add", "ctez_sub"},
    {"eq_ctez_ctez", "lt_ctez_ctez", "gt_ctez_ctez"},
    {"ctez_of_fraction_ceil", "ctez_of_fraction_floor"},
    # LQT
    {"lqt_add", "lqt_sub"},
    {"lqt_zero", "lqt_one"},
    {"lqt_of_fraction_ceil", "lqt_of_fraction_floor"},
    {"geq_lqt_lqt", "eq_lqt_lqt", "lt_lqt_lqt"},
    # TOK
    {"tok_add", "tok_sub", "max_tok"},
    {"tok_zero", "tok_one"},
    {"tok_of_fraction_ceil", "tok_of_fraction_floor"},
    {"geq_tok_tok", "leq_tok_tok", "eq_tok_tok", "gt_tok_tok", "lt_tok_tok"},
    # FIXEDPOINT
    {"fixedpoint_zero", "fixedpoint_one"},
    {"fixedpoint_add", "fixedpoint_sub"},
    {"fixedpoint_of_ratio_ceil", "fixedpoint_of_ratio_floor"},
    # INT
    {"mul_int_int", "sub_int_int", "add_int_int", "div_int_int"},
    {"eq_int_int", "lt_int_int", "gt_int_int", "leq_int_int", "geq_int_int"},
    # NAT
    # {"mul_nat_int"}, # note: there is no sub_nat_int add_nat_int", "div_nat_int"
    # {"eq_nat_int", "lt_nat_int", "gt_nat_int", "leq_nat_int", "geq_nat_int"},
    # TEZ
    {"sub_tez_tez", "add_tez_tez"},
    {"eq_tez_tez", "lt_tez_tez", "gt_tez_tez", "leq_tez_tez", "geq_tez_tez"},
    # RATIO
    {
        "mul_ratio",
        "add_ratio",
        "sub_ratio",
        "div_ratio",
        "min_ratio",
        "max_ratio",
    },
    {
        "lt_ratio_ratio",
        "leq_ratio_ratio",
        "geq_ratio_ratio",
        "gt_ratio_ratio",
        "eq_ratio_ratio",
        # most (all?) of these only appear on tests, but they are here for
        # completeness' sake.
    },
    # BOOL
    {"true", "false"},
]

MUTATION_MAPPINGS = []

# Mappings for function swap mutations
for group in MUTATION_GROUPS:
    for from_mutation in group:
        # Might need other escapes here as well
        escaped = from_mutation.replace(".", "\.")
        from_regex = re.compile(f"^.*(?:\.|\s|=)({from_mutation}).*$")
        to_mutations = {m for m in group if m != from_mutation}
        MUTATION_MAPPINGS += [
            (MutationType.SWAP_FUNCTION, (from_mutation, from_regex, m)) for m in to_mutations
        ]

## Add mapping for integer (tez, nat, int) mutations
# How much to weight integer mutations relative to other mutations
_INTEGER_LITERAL_MUTATION_WEIGHT = 0.5
_INTEGER_LITERAL_REGEX = re.compile('^.*"([\d_]+[a-zA-Z]*)".*$')
for _ in range(0, round(_INTEGER_LITERAL_MUTATION_WEIGHT * len(MUTATION_MAPPINGS))):
    MUTATION_MAPPINGS.append((MutationType.INTEGER_LITERAL, _INTEGER_LITERAL_REGEX))


def mutate_integer_literal(value_src: str) -> str:
    """Mutates an integer literal value from OCaml to a random value

    Works with Ligo.int, Ligo.nat, and Ligo.tez values.

    Args:
        value_src (str): The input value string (e.g. 4mutez or 1_000_000n)

    Returns:
        str: The mutated value string

    >>> mutate_integer_literal("1_000_000mutez")
    "0mutez"
    """
    # Drop known suffixes
    if value_src.endswith("mutez"):
        suffix = "mutez"
    elif value_src.endswith("n"):
        suffix = "n"
    else:
        suffix = ""
    prefix = value_src.rstrip(suffix)

    # Attempt to parse to an int
    value = int(prefix)

    if value_src.endswith("mutez") or value_src.endswith("n"):
        # We're dealing with numbers which are bounded [0, inf)
        if value == 0:
            mutated_value = 1
        else:
            choices = [c for c in [0, 1, value + 1, value - 1] if c != value]
            mutated_value = random.choice(choices)
    else:
        choices = [c for c in [-1, 0, 1, value + 1, value - 1] if c != value]
        mutated_value = random.choice(choices)
    return f"{mutated_value:_}{suffix}"


def mutate(
    src: str, before_regex: re.Pattern, formatter: MutationFormatter
) -> Tuple[bool, Optional[int], Optional[str], Optional[str]]:
    """Mutates an OCaml src file

    Args:
        src (str): The path to the OCaml src file
        before_regex (re.Pattern): Regex with a single capturing group which identifies the mutation site
        formatter (MutationFormatter): A function which generates a mutation for a given source

    Returns:
        A tuple containing a flag indicating whether a mutation was performed, the line mutated, the
        original statement, and the corresponding mutated statement.
    """
    with open(src) as f:
        lines = [l.rstrip("\n") for l in f.readlines()]
    matching_lines = []
    for i, l in enumerate(lines):
        stripped = l.lstrip()
        # Note: this is not super precise and could skip lines with
        # actual code which start with the * operator. This doesn't
        # seem to happen in the checker codebase though...
        if stripped.startswith("(*") or stripped.startswith("*"):
            continue
        # Super basic logic for ignoring assertions. This might be overly
        # aggressive (e.g. in cases with an inline comment talking about assertions), but
        # this doesn't seem to be a common case.
        if "assert" in stripped:
            continue
        # Similar logic for lines which are manually flagged as needing to be ignored
        if "SKIP_MUTATION" in stripped:
            continue
        match = before_regex.match(l)
        if match:
            assert len(match.regs) == 2
            matching_lines.append((i, match.groups(1)[0], match.span(1)))

    if not matching_lines:
        return False, None, None, None
    i_match, before, (start, stop) = matching_lines[random.randint(0, len(matching_lines) - 1)]
    after = formatter(before)
    old_line = lines[i_match]
    new_line = old_line[:start] + after + old_line[stop:]
    lines[i_match] = new_line
    with open(src, "w") as f:
        for line in lines:
            print(line, file=f)
    return True, i_match, before, after


def restore(src: str):
    result = subprocess.run(f"git restore {src}", shell=True, capture_output=True)
    if result.returncode != 0:
        raise Exception(
            f"Failed to git restore {src} with error: "
            + result.stdout.decode()
            + result.stderr.decode()
        )


def test_mutated_src(test_cmd):
    print("Checking validity of modified src code...")
    result = subprocess.run(["dune build"], capture_output=True, shell=True)
    if result.returncode != 0:
        print(
            "Failed to build mutated src with error: "
            + result.stdout.decode()
            + result.stderr.decode()
        )
        print("Skipping.")
        return True
    else:
        print("Running tests...")
        result = subprocess.run([test_cmd], capture_output=True, shell=True)
        tests_fail = True
        if result.returncode == 0:
            tests_fail = False
        # print(result.stderr)
        print("Done.")
        return tests_fail


@contextmanager
def do_mutation(args):
    # Randomly pick a module
    modules = list(args.modules)
    random.shuffle(modules)
    for module in modules:
        print(f"Attempting to mutate {module}...")
        possible_mutations = [m for m in MUTATION_MAPPINGS]
        random.shuffle(possible_mutations)
        for mutation_type, mutation_meta in possible_mutations:
            if mutation_type == MutationType.SWAP_FUNCTION:
                mutation_from, mutation_from_regex, mutation_to = mutation_meta
                formatter = lambda x: mutation_to
            elif mutation_type == MutationType.INTEGER_LITERAL:
                mutation_from_regex = mutation_meta
                mutation_from = "literal_number"
                mutation_to = "literal_number"
                formatter = mutate_integer_literal
            else:
                raise Exception(f"Unknown mutation type {mutation_type}")

            print(f"  Trying mutation: {mutation_from} -> {mutation_to}")
            found_site_to_mutate, line, before, after = mutate(
                module, mutation_from_regex, formatter
            )
            if found_site_to_mutate:
                "Successfully mutated source."
                break
        if found_site_to_mutate:
            break
        print(
            f"Could not find any known sites to mutate in module {module}. "
            "Trying another module..."
        )

    if not found_site_to_mutate:
        raise Exception(
            "Tried all possible combinations of modules and mutations and could not find a matching name"
        )
    tests_failed = test_mutated_src(args.test)
    yield (module, mutation_type, before, after, line), tests_failed
    restore(src)


# Command line interface
parser = argparse.ArgumentParser(description="Mutates Checker source code.")
parser.add_argument(
    "modules",
    metavar="MODULE",
    type=str,
    nargs="+",
    help="Module to mutate (eg. 'burrow.ml')",
)

parser.add_argument(
    "--test",
    metavar="COMMAND",
    type=str,
    help="Command to execute for testing the change",
)

parser.add_argument("--num-mutations", type=int, help="Number of mutations to test", default=25)

parser.add_argument("--seed", type=int, help="Random seed", default=42)

if __name__ == "__main__":
    args = parser.parse_args()
    random.seed(args.seed)

    report = {}
    for i in range(args.num_mutations):
        print(f"Running mutation {i+1} / {args.num_mutations}...")
        # Using a context manager here as a quick and dirty way to ensure that mutations are removed
        # when the script exits.
        with do_mutation(args) as mutation_result:
            (
                src,
                mutation_type,
                mutation_from,
                mutation_to,
                line,
            ), tests_failed = mutation_result
        if src not in report:
            report[src] = {}
        report[src][(mutation_type, mutation_from, mutation_to, line)] = tests_failed
        srcs = list(report.keys())
        srcs.sort()

    pprint(report)

    print_lines = [
        "==================================================================================",
        "CASES NOT CAUGHT BY TESTS:",
        "==================================================================================",
        f"{'SRC':<20} {'TYPE':<20} {'MUTATION_FROM':<15} -> {'MUTATION_TO':15} L{'LINE':<5}",
    ]
    for src in srcs:
        mutations = report[src]
        for (mutation_details, tests_failed) in mutations.items():
            mutation_type, mutation_from, mutation_to, line_number = mutation_details
            if not tests_failed:
                print_lines.append(
                    f"{src:<20} {mutation_type.name:<20} {mutation_from:<15} -> {mutation_to:15} L{(line_number+1):<5}"
                )
    for l in print_lines:
        print(l)
