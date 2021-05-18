from typing import Optional
import click

from client import checker as checker_client
import pytezos
import os
from pathlib import Path
from dataclasses import dataclass
from marshmallow import Schema, fields
from marshmallow.decorators import post_load
import portpicker

CONFIG_FILE = Path.home().joinpath(Path(".checker"))


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
@click.option("--config", "config_file", default=CONFIG_FILE, help="path to CLI config file")
def cli(ctx, config_file):
    """Checker command line utilities"""
    config_file = Path(config_file)
    if config_file.exists():
        config = Config.load(config_file)
    else:
        config = Config(path=config_file)
        config.dump()
    ctx.obj = config


@cli.group()
def sandbox():
    """Interact with the Tezos sandbox for Checker"""
    pass


@sandbox.command()
@click.pass_obj
def start(config: Config):
    """Starts the sandbox"""
    if checker_client.is_sandbox_container_running(Config.sandbox_container):
        click.echo(f"Container {Config.sandbox_container} is already running.")
    else:
        click.echo("Starting sandbox container...")
        if not portpicker.is_port_free(config.sandbox_port):
            click.echo(f"Selected host port {config.sandbox_port} for sandbox.")
            config.sandbox_port = portpicker.pick_unused_port()
        config.dump()
        checker_client.start_sandbox(config.sandbox_container, config.sandbox_port)
        click.echo("Sandbox started.")


@sandbox.command()
@click.pass_obj
def stop(config: Config):
    """Stops the sandbox"""
    if not checker_client.is_sandbox_container_running(Config.sandbox_container):
        click.echo(f"Container {Config.sandbox_container} is already stopped.")
    else:
        click.echo("Stopping sandbox container...")
        checker_client.stop_sandbox(config.sandbox_container)
        click.echo("Sandbox stopped.")


@cli.group()
@click.option("--address")
@click.option("--port", type=int)
@click.option("--key")
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
@click.pass_obj
def checker(config: Config, checker_dir, oracle, ctez):
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
    checker = checker_client.deploy_checker(
        client, checker_dir, oracle=config.oracle_address, ctez=config.ctez_address
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
@click.pass_obj
def ctez(config: Config, ctez_dir):
    """
    Deploy a ctez contract (dev only)
    """
    shell = f"{config.tezos_address}:{config.tezos_port}"
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    ctez = checker_client.deploy_ctez(
        client,
        ctez_dir=ctez_dir,
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
@click.pass_obj
def mock_oracle(config: Config, oracle_src):
    """
    Deploy the mock oracle contract (dev only)
    """
    shell = f"{config.tezos_address}:{config.tezos_port}"
    click.echo(f"Connecting to tezos node at: {shell}")
    client = pytezos.pytezos.using(shell=shell, key=config.tezos_key)
    oracle = checker_client.deploy_contract(
        client,
        source_file=oracle_src,
        initial_storage=(client.key.public_key_hash(), 1000000),
    )
    click.echo(f"mock oracle contract deployed with address: {oracle.context.address}")
    config.oracle_address = oracle.context.address
    config.dump()


if __name__ == "__main__":
    cli()
