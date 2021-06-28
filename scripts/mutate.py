from contextlib import contextmanager
import os
import random
import re
import subprocess
from pprint import pprint
from typing import Optional, Tuple

random.seed(42)

MODULES = ["burrow.ml", "checker.ml", "parameters.ml", "cfmm.ml"]

# Each group contains functions with the same type signature which can be swapped
# and still allow the program to compile.
MUTATION_GROUPS = [
    ## MATH
    # KIT
    {"kit_add", "kit_sub", "kit_min", "kit_max"},
    {"gt_kit_kit", "geq_kit_kit", "eq_kit_kit", "lt_kit_kit"},
    # INT
    {"mul_int_int", "sub_int_int", "add_int_int", "div_int_int"},
    {"eq_int_int", "lt_int_int", "gt_int_int", "leq_int_int", "geq_int_int"},
    # NAT
    {"mul_nat_int", "sub_nat_int", "add_nat_int", "div_nat_int"},
    {"eq_nat_int", "lt_nat_int", "gt_nat_int", "leq_nat_int", "geq_nat_int"},
    # TEZ
    {"mul_tez_int", "sub_tez_int", "add_tez_int", "div_tez_int"},
    {"eq_tez_int", "lt_tez_int", "gt_tez_int", "leq_tez_int", "geq_tez_int"},
    # FIXED POINT
    {"fixedpoint_add", "fixedpoint_sub"},
    {"fixedpoint_of_ratio_ceil", "fixedpoint_of_ratio_floor"},
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
    },
]

MUTATION_MAPPINGS = []
for group in MUTATION_GROUPS:
    for from_mutation in group:
        # Might need other escapes here as well
        escaped = from_mutation.replace(".", "\.")
        from_regex = re.compile(f"^.*(?:\.|\s|=)({from_mutation}).*$")
        to_mutations = {m for m in group if m != from_mutation}
        MUTATION_MAPPINGS += [(from_mutation, from_regex, m) for m in to_mutations]


def mutate(
    src: str, before_regex: re.Pattern, after: str
) -> Tuple[bool, Optional[int]]:
    with open(src) as f:
        lines = [l.rstrip("\n") for l in f.readlines()]
    matching_lines = []
    for i, l in enumerate(lines):
        match = before_regex.match(l)
        if match:
            assert len(match.regs) == 2
            matching_lines.append((i, match.span(1)))

    if not matching_lines:
        return False, None
    i_match, (start, stop) = matching_lines[random.randint(0, len(matching_lines) - 1)]
    old_line = lines[i_match]
    new_line = old_line[:start] + after + old_line[stop:]
    lines[i_match] = new_line
    with open(src, "w") as f:
        for line in lines:
            print(line, file=f)
    return True, i_match


def restore(src: str):
    subprocess.run(["git", "restore", src], check=True)


def test_mutated_src():
    print("Checking validity of modified src code...")
    result = subprocess.run(["dune", "build"], capture_output=True)
    if result.returncode != 0:
        raise Exception(
            "Failed to build mutated src with error: " + result.stderr.decode()
        )

    print("Running tests...")
    result = subprocess.run(["make", "test"], capture_output=True)
    tests_fail = True
    if result.returncode == 0:
        tests_fail = False
    # print(result.stderr)
    print("Done.")
    return tests_fail


@contextmanager
def do_mutation():
    print(f"Running mutation {i+1} / {n_mutations}...")
    # Randomly pick a module
    modules = [m for m in MODULES]
    random.shuffle(modules)
    for module in modules:
        src = os.path.join("src", module)
        print(f"Attempting to mutate {src}...")
        possible_mutations = [m for m in MUTATION_MAPPINGS]
        random.shuffle(possible_mutations)
        for mutation_from, mutation_from_regex, mutation_to in possible_mutations:
            print(f"  Trying mutation: {mutation_from} -> {mutation_to}")
            found_site_to_mutate, line = mutate(src, mutation_from_regex, mutation_to)
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
    tests_failed = test_mutated_src()
    yield (src, mutation_from, mutation_to, line), tests_failed
    restore(src)


if __name__ == "__main__":
    n_mutations = 1000
    report = {}
    for i in range(n_mutations):
        # Using a context manager here as a quick and dirty way to ensure that mutations are removed
        # when the script exits.
        with do_mutation() as mutation_result:
            (src, mutation_from, mutation_to, line), tests_failed = mutation_result
        if src not in report:
            report[src] = {}
            report[src][(mutation_from, mutation_to, line)] = tests_failed
        srcs = list(report.keys())
        srcs.sort()

    pprint(report)

    print_lines = [
        "==============================================================",
        "CASES NOT CAUGHT BY TESTS:",
        "==============================================================",
        "SRC                    MUTATION_FROM -> MUTATION_TO    LINE_NO",
    ]
    for src in srcs:
        mutations = report[src]
        for (mutation_details, tests_failed) in mutations.items():
            mutation_from, mutation_to, line_number = mutation_details
            if not tests_failed:
                print_lines.append(
                    f"{src:<24} {mutation_from:>0} -> {mutation_to} L{line_number}"
                )
    for l in print_lines:
        print(l)
