import os
from github import Github
import click
from github.Repository import Repository
import subprocess
import logging

logging.basicConfig(level=logging.INFO, format="%(message)s")

token = os.environ.get("GITHUB_TOKEN")

CI_CHECK_NAMES = ["build", "e2e"]


def ci_passed(commit_sha: str, repo: Repository) -> bool:
    commit = repo.get_commit(commit_sha)
    # Forcing this lazy list since there shouldn't be so many check runs
    check_runs = []
    for check in CI_CHECK_NAMES:
        check_runs += [r for r in commit.get_check_runs(check_name=check)]
    if not check_runs:
        logging.info(
            f"Commit {commit_sha} has no check runs. Marking CI as not passing."
        )
        return False
    return all(run.conclusion == "success" for run in check_runs)


@click.command()
def main():
    github = Github(token)
    repo = github.get_repo("tezos-checker/checker")
    # List open PRs
    prs = repo.get_pulls(state="open", sort="created", base="master")

    for pr in prs:
        existing_comment = None
        logging.info(f"Checking PR #{pr.number}...")
        # For each PR
        # Filter by those which have passing tests
        if not ci_passed(pr.base.sha, repo) or not ci_passed(pr.head.sha, repo):
            logging.info(f"CI is not passing for open PR #{pr.number}. Skipping.")
            continue
        # Filter PRs with existing comments
        marker = f"bot:{pr.base.sha}:{pr.head.sha}"
        for comment in pr.get_issue_comments():
            if marker in comment.body:
                existing_comment = comment
                break
        # Run comparison script
        cmd = [
            "poetry",
            "run",
            "python",
            "./scripts/artifacts.py",
            "compare-stats",
            "--previous",
            pr.base.sha,
            "--next",
            pr.head.sha,
        ]

        res = subprocess.run(cmd, capture_output=True)
        if res.returncode != 0:
            logging.error(res.stderr)
            logging.info(
                f"Call to artifact script failed for PR #{pr.number}. Skipping."
            )
            continue

        # Create comment
        comment_body = f"{res.stdout.decode()} \n\n[hiddencomment]: {marker}"
        if existing_comment is not None:
            logging.info("Editing existing comment")
            existing_comment.edit(comment_body)
        else:
            logging.info("Posting new comment")
            pr.create_issue_comment(comment_body)


if __name__ == "__main__":
    main()
