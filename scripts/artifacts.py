#!/usr/bin/env python

import io
import sys
import json
import yaml
import zipfile
import os.path
import itertools as it
from pprint import pprint
from collections import namedtuple

import click
import requests
from tqdm import tqdm
import matplotlib.pyplot as plt

ENDPOINT_GRAPHQL = f"https://api.github.com/graphql"
ENDPOINT_REST = f"https://api.github.com"

CommitInfo = namedtuple("CommitInfo", ["message", "ref", "rev", "workflow_ids"])
CommitStats = namedtuple(
    "CommitStats", ["info", "gas_costs", "entrypoint_sizes", "test_coverage"]
)
Diff = namedtuple("Diff", ["key", "previous", "next"])

accessToken = os.getenv("GITHUB_TOKEN")
if not accessToken:
    with open(os.path.expanduser("~/.config/hub")) as f:
        accessToken = yaml.load(f, Loader=yaml.SafeLoader)["github.com"][0][
            "oauth_token"
        ]
headers = {"Authorization": f"Bearer {accessToken}"}
del accessToken


@click.group()
def cli():
    pass


@cli.command()
@click.option("--ref", required=True)
@click.option(
    "--out",
    type=click.Path(file_okay=True, dir_okay=False, writable=True),
    required=True,
)
@click.option("--limit", type=int, default=10)
def plot_stats(out, ref, limit):
    print("Fetching latest commits.", file=sys.stderr)
    infos = fetch_commit_infos(ref, limit=limit)
    stats = list(map(mk_commit_stats, tqdm(infos, desc="Fetching artifacts")))
    stats.reverse()

    fig, axs = plt.subplots(2)
    fig.set_size_inches(14, 10)

    xticks = list(range(0, len(stats)))
    xlabels = [i.info.ref for i in stats]

    # gas costs
    axs[0].set_title("Gas Costs")
    for key in set(it.chain(*[i.gas_costs for i in stats])):
        axs[0].plot(xticks, [i.gas_costs.get(key) for i in stats], label=key)

    # entrypoint sizes
    axs[1].set_title("Entrypoint Sizes")
    for key in set(it.chain(*[i.entrypoint_sizes for i in stats])):
        axs[1].plot(xticks, [i.entrypoint_sizes.get(key) for i in stats], label=key)

    # save the graph
    for ax in axs:
        ax.set_xticks(xticks)
        ax.set_xticklabels(xlabels, rotation=65)
        ax.legend(bbox_to_anchor=(1.05, 1), loc="upper left")

    plt.tight_layout()
    plt.savefig(out)


@cli.command()
@click.option("--previous", required=True)
@click.option("--next", required=True)
def compare_stats(previous, next):
    previous_stats = mk_commit_stats(fetch_commit_info(previous))
    next_stats = mk_commit_stats(fetch_commit_info(next))

    print_diffs(
        mk_diffs(previous_stats.gas_costs, next_stats.gas_costs),
        title="Gas costs",
        header_previous=previous_stats.info.ref,
        header_next=next_stats.info.ref,
    )

    print_diffs(
        mk_diffs(previous_stats.entrypoint_sizes, next_stats.entrypoint_sizes),
        title="Entrypoint sizes",
        header_previous=previous_stats.info.ref,
        header_next=next_stats.info.ref,
    )

    print_diffs(
        mk_diffs(previous_stats.test_coverage, next_stats.test_coverage),
        title="Test coverage",
        header_previous=previous_stats.info.ref,
        header_next=next_stats.info.ref,
        total_key="TOTAL",
    )


# From the given $ref (usually a commit hash or a branch name), fetch the latest $limit commits
# (from children to parents) and their workflow ids.
def fetch_commit_infos(ref, *, limit):
    query = """
    query($ref:String!, $limit:Int!) {
      repository(owner: "tezos-checker", name: "checker") {
        object(expression: $ref) {
          ... on Commit {
            history(first: $limit) {
              nodes {
                message
                oid
                statusCheckRollup {
                  contexts(first: 10) {
                    edges {
                      node {
                        ... on CheckRun {
                          checkSuite {
                            workflowRun {
                              databaseId
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  """

    vars = {"ref": ref, "limit": limit}

    ret = requests.post(
        ENDPOINT_GRAPHQL, json={"query": query, "variables": vars}, headers=headers
    )
    ret.raise_for_status()
    ret = ret.json()

    commit_infos = [
        CommitInfo(
            message=get_first_line(commit["message"]),
            ref=ref if i == 0 else f"{ref}~{i}",
            rev=commit["oid"],
            workflow_ids=[
                check["node"]["checkSuite"]["workflowRun"]["databaseId"]
                for check in commit["statusCheckRollup"]["contexts"]["edges"]
            ]
            if commit["statusCheckRollup"]
            else [],
        )
        for i, commit in enumerate(
            ret["data"]["repository"]["object"]["history"]["nodes"]
        )
        if commit["statusCheckRollup"]
    ]

    return commit_infos


def fetch_commit_info(expr):
    return fetch_commit_infos(expr, limit=1)[0]


def mk_commit_stats(info):
    stats = download_stats(info.workflow_ids)

    gas_costs = stats.get("gas-costs", [{}])[0]
    # TODO: Gas costs are strings for no reason.
    gas_costs = {k: int(v) for k, v in gas_costs.items()}

    entrypoint_sizes = stats.get("entrypoint-sizes", [{}])[0]

    # NOTE: These are represented as floating-point numbers, but the existing
    # infrastructure seems to be working OK with them.
    test_coverage = stats.get("test-coverage", [{}])[0]

    return CommitStats(
        info=info,
        gas_costs=gas_costs,
        entrypoint_sizes=entrypoint_sizes,
        test_coverage=test_coverage,
    )


# Given a list of workflow_ids, download the JSON files within the artifacts.
def download_stats(workflow_ids):
    stats_urls = []

    # fetch all artifacts archive urls
    for workflow_id in workflow_ids:
        url = (
            ENDPOINT_REST
            + f"/repos/tezos-checker/checker/actions/runs/{workflow_id}/artifacts"
        )
        ret = requests.get(url, headers=headers)
        ret.raise_for_status()
        ret = ret.json()
        stats_urls.extend(i["archive_download_url"] for i in ret["artifacts"])

    stat_results = {}
    for url in stats_urls:
        req = requests.get(url, headers=headers, stream=True)
        buf = io.BytesIO(req.content)
        with zipfile.ZipFile(buf) as zip:
            for info in zip.infolist():
                name, ext = os.path.splitext(info.filename)
                if ext == ".json":
                    ret = json.loads(zip.read(info).decode("utf-8"))
                    stat_results.setdefault(name, []).append(ret)

    return stat_results


def get_first_line(s):
    ls = s.splitlines()
    return ls[0] if ls else ""


def diff_change(diff):
    if diff.previous is None:
        return diff.next
    elif diff.next is None:
        return -diff.previous
    else:
        return diff.next - diff.previous


def mk_diffs(prevs, nexts):
    diffs = [Diff(k, prevs.get(k), nexts.get(k)) for k in set(it.chain(prevs, nexts))]

    diffs.sort(key=lambda i: abs(diff_change(i)), reverse=True)
    return diffs


def print_diffs(diffs, *, title, header_previous, header_next, total_key=None):
    diffs = [i for i in diffs if diff_change(i) != 0]
    if not diffs:
        print(f"{title}: No change.")
    else:
        title_len = max(len(title), *[len(i.key) for i in diffs])
        total_entry = None

        print(
            f"| {title.ljust(title_len)} | {header_previous: <10} | {header_next: <10} | {'Diff': <10} |"
        )
        print(f"| {'-'*title_len} | {'-'*10} | {'-'*10} | {'-'*10} |")
        for diff in diffs:
            if (total_key is None) or (diff.key != total_key):
                print(
                    f"| {diff.key.ljust(title_len)} | {str(diff.previous): <10} | {str(diff.next): <10} | {diff_change(diff): <10} |"
                )
            else:
                total_entry = diff
        if total_entry is not None:
            print(
                f"| **{total_entry.key.ljust(title_len)}** | {str(total_entry.previous): <10} | {str(total_entry.next): <10} | {diff_change(total_entry): <10} |"
            )
        print()


if __name__ == "__main__":
    cli()
