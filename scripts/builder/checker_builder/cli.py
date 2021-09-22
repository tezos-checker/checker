from pathlib import Path
import click

from checker_builder import config

# Mapping of generated src modules to their templates
GENERATE_SRCS = {
    "tok.ml": "tok.ml.jinja",
    "constants2.ml": "constants.ml.jinja",
}


@click.group()
def cli():
    """Checker build utilities"""
    pass


@cli.command()
def generate():
    """Run code generation"""
    checker_config = config.load_checker_config()
    env = config.load_template_env()
    for src, template_name in GENERATE_SRCS.items():
        template = env.get_template(template_name)
        config.generate_src_module(
            Path("./src").joinpath(src),
            template,
            checker_config,
        )


if __name__ == "__main__":
    cli()
