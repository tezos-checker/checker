import click

from client import checker
import pytezos
import os
from pathlib import Path
from dataclasses import dataclass
from marshmallow import Schema, fields
from marshmallow.decorators import post_load

CONFIG_FILE = Path.home().joinpath(Path(".checker"))


@dataclass
class Config:
    path: str
    sandbox_container: str = "checker-sandbox-node"

    @staticmethod
    def load(file: Path):
        with file.open("r") as f:
            return ConfigSchema().loads(f.read())

    def dump(self):
        with open(self.path, "w") as f:
            f.write(ConfigSchema().dumps(self))


class ConfigSchema(Schema):
    path = fields.String()
    sandbox_container = fields.String()

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
        config = Config(config_file)
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
    if checker.is_sandbox_container_running(Config.sandbox_container):
        click.echo(f"Container {Config.sandbox_container} is already running.")
    else:
        click.echo("Starting sandbox container...")
        checker.start_sandbox(config.sandbox_container)
        click.echo("Sandbox started.")


@sandbox.command()
@click.pass_obj
def stop(config: Config):
    """Stops the sandbox"""
    if not checker.is_sandbox_container_running(Config.sandbox_container):
        click.echo(f"Container {Config.sandbox_container} is already stopped.")
    else:
        click.echo("Stopping sandbox container...")
        checker.stop_sandbox(config.sandbox_container)
        click.echo("Sandbox stopped.")


# @cli.command
# @click.argument("--address")
# @click.option("--port", required=True)
# @click.option("--key", required=True)
# @click.pass_context
# def deploy(ctx, address, port, key):
#     ctx.tz = pytezos.pytezos.using(
#         shell=f"{address}:{port}".format(port),
#         key=key,
#     )


# @deploy.command()
# @click.pass_context
# def checker(ctx):
#     pass


# @deploy.command()
# @click.pass_context
# def ctez(ctx):
#     pass


if __name__ == "__main__":
    cli()
