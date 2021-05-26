import json
import os
from dataclasses import dataclass
from pathlib import Path

import click
import pytezos
from marshmallow import Schema, fields
from marshmallow.decorators import post_load

from checker_client import checker as checker_lib
from pytezos.operation import MAX_OPERATIONS_TTL

CONFIG_FILE_BASE = Path(".checker")
CONFIG_DIR = os.getenv("XDG_CONFIG_HOME")
if not CONFIG_DIR:
    CONFIG_DIR = Path.home().joinpath(".config")
else:
    CONFIG_DIR = Path(CONFIG_DIR)
CONFIG_FILE = CONFIG_DIR.joinpath(CONFIG_FILE_BASE)


# FIXME: This feels wrong. Depending on the answer to the below issue, we might
# end up needing to update this value.
# https://github.com/baking-bad/pytezos/issues/229
SANDBOX_TTL = MAX_OPERATIONS_TTL

# FIXME: This whole function is bit of a hack. See the comment above.
def _patch_operation_ttl(node_address: str) -> int:
    # Checks if the node is running on the local loopback and if so,
    # returns a shorter ttl to allow operations on the sandbox to run
    # as fast as possible.
    if any([loopback in node_address for loopback in ("localhost", "127.0.0.1")]):
        ttl = SANDBOX_TTL
        print(f"Detected sandbox address. Using operation ttl={ttl}")
    else:
        ttl = None
    return ttl


@dataclass
class Config:
    path: str
    sandbox_port: int = 18731
    sandbox_container: str = "checker-sandbox-node"
    tezos_address: str = "http://127.0.0.1"
    tezos_port: int = 18731
    tezos_key: str = "edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt"  # bob's private key from "edobox info"
    ctez_address: str = ""
    oracle_address: str = ""
    checker_address: str = ""

    @staticmethod
    def load(file: Path):
        with file.open("r") as f:
            return ConfigSchema().loads(f.read())

    def dump(self):
        with open(self.path, "w") as f:
            f.write(ConfigSchema().dumps(self))


class ConfigSchema(Schema):
    path = fields.String(required=True)
    sandbox_container = fields.String()
    sandbox_port = fields.Int()
    sandbox_container = fields.String()
    tezos_address = fields.String()
    tezos_port = fields.Int()
    tezos_key = fields.String()
    ctez_address = fields.String()
    oracle_address = fields.String()
    checker_address = fields.String()

    @post_load
    def make(self, data, **kwargs):
        return Config(**data)


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
@click.option("--port", type=int, help="The port on which the sandbox should listen")
@click.pass_obj
def start(config: Config, port=None):
    """Starts the sandbox"""
    if checker_lib.is_sandbox_container_running(Config.sandbox_container):
        click.echo(f"Container {Config.sandbox_container} is already running.")
    else:
        if port is not None:
            config.sandbox_port = port
            # Note: also setting the default tezos client port here as well which optimizes
            # the experience for people primarily working with the sandbox. If this proves
            # annoying we can remove this line.
            config.tezos_port = port
            config.dump()
        click.echo(f"Starting sandbox container using host port {config.sandbox_port}...")
        checker_lib.start_sandbox(
            config.sandbox_container,
            config.sandbox_port,
            wait_for_level=(MAX_OPERATIONS_TTL - SANDBOX_TTL),
        )
        click.echo("Sandbox started.")


@sandbox.command()
@click.pass_obj
def stop(config: Config):
    """Stops the sandbox"""
    if not checker_lib.is_sandbox_container_running(Config.sandbox_container):
        click.echo(f"Container {Config.sandbox_container} is already stopped.")
    else:
        click.echo("Stopping sandbox container...")
        checker_lib.stop_sandbox(config.sandbox_container)
        click.echo("Sandbox stopped.")


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
    "checker_dir",
    type=str,
    help="Checker michelson src directory",
    default="generated/michelson",
    show_default=True,
)
@click.option("--oracle", type=str, help="Oracle contract address")
@click.option("--ctez", type=str, help="ctez contract address")
@click.option(
    "--wait",
    type=int,
    default=100,
    show_default=True,
    help="The number of blocks to wait for an operation group to finish",
)
@click.option(
    "--token-metadata",
    type=click.Path(exists=True),
    help="optional JSON file containing the TZIP-12 token_metadata.",
)
@click.pass_obj
def checker(config: Config, checker_dir, oracle, ctez, wait, token_metadata):
    """
    Deploy checker. Requires addresses for oracle and ctez contracts.
    """
    if not config.oracle_address and not oracle:
        raise ValueError(
            "Oracle address was neither specified in the CLI config or provided as an argument."
        )
    if not config.ctez_address and not ctez:
        raise ValueError(
            "ctez address was neither specified in the CLI config or provided as an argument."
        )
    if oracle:
        config.oracle_address = oracle
    if ctez:
        config.ctez_address = ctez
    shell = f"{config.tezos_address}:{config.tezos_port}"
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    checker = checker_lib.deploy_checker(
        client,
        checker_dir,
        oracle=config.oracle_address,
        ctez=config.ctez_address,
        num_blocks_wait=wait,
        ttl=_patch_operation_ttl(config.tezos_address),
        token_metadata_file=token_metadata,
    )
    click.echo(f"Checker contract deployed with address: {checker.context.address}")
    config.checker_address = checker.context.address
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "ctez_dir",
    type=str,
    help="ctez michelson src directory",
    default="vendor/ctez",
    show_default=True,
)
@click.option(
    "--wait",
    type=int,
    default=100,
    show_default=True,
    help="The number of blocks to wait for an operation group to finish",
)
@click.pass_obj
def ctez(config: Config, ctez_dir, wait):
    """
    Deploy a ctez contract (dev only)
    """
    shell = f"{config.tezos_address}:{config.tezos_port}"
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    ctez = checker_lib.deploy_ctez(
        client, ctez_dir=ctez_dir, ttl=_patch_operation_ttl(config.tezos_address)
    )
    click.echo(f"ctez contract deployed with address: {ctez['fa12_ctez'].context.address}")
    config.ctez_address = ctez["fa12_ctez"].context.address
    config.dump()


@deploy.command()
@click.option(
    "--src",
    "oracle_src",
    type=str,
    help="oracle michelson src file",
    default="util/mock_oracle.tz",
    show_default=True,
)
@click.option(
    "--wait",
    type=int,
    default=100,
    show_default=True,
    help="The number of blocks to wait for an operation group to finish",
)
@click.pass_obj
def mock_oracle(config: Config, oracle_src, wait):
    """
    Deploy the mock oracle contract (dev only)
    """
    shell = f"{config.tezos_address}:{config.tezos_port}"
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    oracle = checker_lib.deploy_contract(
        client,
        source_file=oracle_src,
        initial_storage=(client.key.public_key_hash(), 1000000),
        num_blocks_wait=wait,
        ttl=_patch_operation_ttl(config.tezos_address),
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
