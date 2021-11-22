import json
import logging
import os
from dataclasses import dataclass
from pathlib import Path
import time
from urllib.parse import urlparse, urlunparse
from checker_builder.config import CheckerRepo

import click
import pytezos
from marshmallow import Schema, fields
from marshmallow.decorators import post_load
from pytezos.operation import MAX_OPERATIONS_TTL

from checker_client import checker as checker_lib

CONFIG_FILE_BASE = Path(".checker")
CONFIG_DIR = os.getenv("XDG_CONFIG_HOME")
if not CONFIG_DIR:
    CONFIG_DIR = Path.home().joinpath(".config")
else:
    CONFIG_DIR = Path(CONFIG_DIR)
CONFIG_FILE = CONFIG_DIR.joinpath(CONFIG_FILE_BASE)

SANDBOX_TTL = MAX_OPERATIONS_TTL


def construct_url(address: str, port: int):
    url = urlparse(address)
    if url.port is not None:
        raise ValueError(
            "Cannot specify a port in the host url. Explicitly pass a port via --port instead."
        )
    url = url._replace(netloc=f"{url.netloc}:{port}")
    return urlunparse(url)


@dataclass
class Config:
    path: str
    tezos_address: str = "http://127.0.0.1"
    tezos_port: int = 20000
    tezos_key: str = "edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt"  # bob's private key from "flobox info"
    ctez_fa12_address: str = ""
    ctez_cfmm_address: str = ""
    oracle_address: str = ""
    checker_address: str = ""
    wtez_address: str = ""
    wctez_address: str = ""
    mock_fa2_address: str = ""

    @staticmethod
    def load(file: Path):
        with file.open("r") as f:
            return ConfigSchema().loads(f.read())

    def dump(self):
        with open(self.path, "w") as f:
            f.write(ConfigSchema().dumps(self, indent=4))


class ConfigSchema(Schema):
    path = fields.String(required=True)
    tezos_address = fields.String()
    tezos_port = fields.Int()
    tezos_key = fields.String()
    ctez_fa12_address = fields.String()
    ctez_cfmm_address = fields.String()
    oracle_address = fields.String()
    checker_address = fields.String()
    wtez_address = fields.String()
    wctez_address = fields.String()
    mock_fa2_address = fields.String()

    @post_load
    def make(self, data, **kwargs):
        return Config(**data)


# FIXME: Might be able to remove this now that we use our own inject() implementation
# TODO: This whole function is bit of a hack. Not sure if there is a better way
# of doing this though.
def _patch_operation_ttl(config: Config) -> int:
    # Checks if the node is running on the local loopback and if so,
    # returns a shorter ttl to allow operations on the sandbox to run
    # as fast as possible.
    if any([loopback in config.tezos_address for loopback in ("localhost", "127.0.0.1")]):
        ttl = SANDBOX_TTL
        print(f"Detected sandbox address. Using operation ttl={ttl}")
    else:
        ttl = None
    return ttl


@click.group()
@click.pass_context
@click.option(
    "--config",
    "config_file",
    default=CONFIG_FILE,
    help="path to CLI config file",
    show_default=True,
)
def cli(ctx, config_file):
    """Checker command line utilities

    By default this tool saves arguments and command
    outputs locally (see the --config flag). This
    allows the output of commands to be used as the default
    arguments for subsequent commands. The current defaults
    can be viewed using `show-config`.
    """
    config_file = Path(config_file)
    # Ensure directory exists
    if not config_file.parent.exists():
        config_file.parent.mkdir(parents=True)
    if config_file.exists():
        config = Config.load(config_file)
    else:
        config = Config(path=config_file)
        config.dump()
    ctx.obj = config


@cli.group()
def sandbox():
    """Interact with the local Tezos sandbox for Checker"""
    pass


@sandbox.command()
@click.pass_obj
def start(config: Config):
    """Starts the sandbox interactively. Runs until interrupted."""
    port = 20000
    # Note: also setting the default tezos client port here as well which optimizes
    # the experience for people primarily working with the sandbox. If this proves
    # annoying we can remove this line.
    config.tezos_port = port
    config.dump()
    click.echo(f"Starting sandbox container using host port {port}...")
    teardown = None
    try:
        _, teardown = checker_lib.start_sandbox(
            "checker-sandbox",
            port,
            wait_for_level=(MAX_OPERATIONS_TTL - SANDBOX_TTL) + 2,
        )
        click.echo("Sandbox started. Running until cancelled...")
        while True:
            time.sleep(1)
    except Exception as e:
        if teardown is not None:
            teardown()
        raise e


@cli.group()
@click.option("--address", help="Address of tezos node for tezos-client")
@click.option("--port", type=int, help="Port of tezos node for tezos-client")
@click.option("--key", help="Private key for tezos-client (file or contents)")
@click.pass_obj
def deploy(config: Config, address=None, port=None, key=None):
    """Deploy Checker and supporting contracts"""
    # Update config with any specified options
    if address:
        config.tezos_address = address
    if port:
        config.tezos_port = port
    if key:
        config.tezos_key = key
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "repo_path",
    type=str,
    help="Path to the checker repo",
    default=".",
    show_default=True,
)
@click.option("--oracle", type=str, help="Oracle contract address")
@click.option("--wtez", type=str, help="Wrapped tez contract address")
@click.option("--ctez_fa12", type=str, help="ctez FA1.2 contract address")
@click.option("--ctez_cfmm", type=str, help="ctez CFMM contract address")
@click.option("--wctez", type=str, help="Wrapped ctez contract address")
@click.pass_obj
def checker(config: Config, repo_path, oracle, wtez, ctez_fa12, ctez_cfmm, wctez):
    """
    Deploy checker. Requires addresses for oracle and ctez contracts.
    """
    if not config.oracle_address and not oracle:
        raise ValueError(
            "Oracle address was neither specified in the CLI config nor provided as an argument."
        )
    if not config.wtez_address and not wtez:
        raise ValueError(
            "Wrapped tez address was neither specified in the CLI config nor provided as an argument."
        )
    if not config.ctez_fa12_address and not ctez_fa12:
        raise ValueError(
            "ctez fa12 address was neither specified in the CLI config nor provided as an argument."
        )
    if not config.ctez_cfmm_address and not ctez_cfmm:
        raise ValueError(
            "ctez cfmm address was neither specified in the CLI config nor provided as an argument."
        )
    if not config.wctez_address and not wctez:
        raise ValueError(
            "Wrapped ctez address was neither specified in the CLI config nor provided as an argument."
        )
    if oracle:
        config.oracle_address = oracle
    if ctez_fa12:
        config.ctez_fa12_address = ctez_fa12
    if ctez_cfmm:
        config.ctez_cfmm_address = ctez_cfmm
    if wtez:
        config.wtez_address = wtez
    if wctez:
        config.wctez = wctez

    shell = construct_url(config.tezos_address, config.tezos_port)
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    client.loglevel = logging.WARNING
    checker = checker_lib.deploy_checker(
        client,
        CheckerRepo(repo_path),
        oracle=config.oracle_address,
        collateral_fa2=config.wtez_address,  # FIXME: only TEZ deployment at the moment
        cfmm_token_fa2=config.wctez_address,  # FIXME: only TEZ deployment at the moment
        ctez_cfmm=config.ctez_cfmm_address,
        wctez=config.wctez,
        ttl=_patch_operation_ttl(config),
    )
    click.echo(f"Checker contract deployed with address: {checker.context.address}")
    config.checker_address = checker.context.address
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "repo_path",
    type=str,
    help="Path to the checker repo",
    default=".",
    show_default=True,
)
@click.pass_obj
def wtez(config: Config, repo_path):
    """
    Deploy Tez FA2 wrapper contract.
    """
    shell = construct_url(config.tezos_address, config.tezos_port)
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    client.loglevel = logging.WARNING
    wrapper = checker_lib.deploy_wtez(
        client,
        CheckerRepo(repo_path),
        ttl=_patch_operation_ttl(config),
    )
    click.echo(f"Tez wrapper contract deployed with address: {wrapper.context.address}")
    config.wtez_address = wrapper.context.address
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "repo_path",
    type=str,
    help="Path to the checker repo",
    default=".",
    show_default=True,
)
@click.option("--ctez_fa12", type=str, help="ctez FA1.2 contract address")
@click.pass_obj
def wrapped_ctez(config: Config, repo_path, ctez_fa12):
    """
    Deploy wctez contract (FA2-wrapped ctez).
    """
    if not config.ctez_fa12_address and not ctez_fa12:
        raise ValueError(
            "ctez fa12 address was neither specified in the CLI config nor provided as an argument."
        )
    if ctez_fa12:
        config.ctez_fa12_address = ctez_fa12
    shell = construct_url(config.tezos_address, config.tezos_port)
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    client.loglevel = logging.WARNING
    wctez = checker_lib.deploy_wctez(
        client,
        CheckerRepo(repo_path),
        config.ctez_fa12_address,
        ttl=_patch_operation_ttl(config),
    )
    click.echo(f"wctez contract deployed with address: {wctez.context.address}")
    config.wctez_address = wctez.context.address
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "repo_path",
    type=str,
    help="Path to the checker repo",
    default=".",
    show_default=True,
)
@click.pass_obj
def mock_fa2(config: Config, repo_path):
    """
    Deploy the mock FA2 contract.
    """
    shell = construct_url(config.tezos_address, config.tezos_port)
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    client.loglevel = logging.WARNING
    mockFA2 = checker_lib.deploy_mockFA2(
        client,
        CheckerRepo(repo_path),
        ttl=_patch_operation_ttl(config),
    )
    click.echo(f"mock FA2 contract deployed with address: {mockFA2.context.address}")
    config.mock_fa2_address = mockFA2.context.address
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "repo_path",
    type=str,
    help="Path to the checker repo",
    default=".",
    show_default=True,
)
@click.pass_obj
def ctez(config: Config, repo_path):
    """
    Deploy a ctez contract (dev only)
    """
    shell = construct_url(config.tezos_address, config.tezos_port)
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    client.loglevel = logging.WARNING
    ctez = checker_lib.deploy_ctez(
        client, repo=CheckerRepo(repo_path), ttl=_patch_operation_ttl(config)
    )
    click.echo(
        f"ctez contract deployed with FA1.2 address: {ctez['fa12_ctez'].context.address} and cfmm address: {ctez['cfmm'].context.address}"
    )
    config.ctez_fa12_address = ctez["fa12_ctez"].context.address
    config.ctez_cfmm_address = ctez["cfmm"].context.address
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "repo_path",
    type=str,
    help="Path to the checker repo",
    default=".",
    show_default=True,
)
@click.pass_obj
def mock_oracle(config: Config, repo_path):
    """
    Deploy the mock oracle contract (dev only)
    """
    shell = construct_url(config.tezos_address, config.tezos_port)
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    client.loglevel = logging.WARNING
    oracle = checker_lib.deploy_contract(
        client,
        source_file=CheckerRepo(repo_path).mock_oracle_contract,
        initial_storage=(client.key.public_key_hash(), 1000000),
        ttl=_patch_operation_ttl(config),
    )
    click.echo(f"mock oracle contract deployed with address: {oracle.context.address}")
    config.oracle_address = oracle.context.address
    config.dump()


@cli.command()
@click.pass_obj
def show_config(config):
    """Show the current CLI config"""
    click.echo(json.dumps(ConfigSchema().dump(config), indent=4, sort_keys=True))


if __name__ == "__main__":
    cli()
