from pathlib import Path
import click
from checker_builder import config

# Mapping of generated src modules to their templates
GENERATE_SRCS = {
    "constants.ml": "constants.ml.jinja",
    "burrowOrigination.ml": "burrowOrigination.ml.jinja",
}

# Template used for all token modules
TOKEN_TEMPLATE = "genericToken.ml.jinja"
# Mapping of token config fields to their corresponding src modules
TOKEN_SRCS = {"collateral": "tok.ml", "kit": "kit.ml", "liquidity": "lqt.ml"}

DRIFT_SRC = "driftDerivative.ml"
DRIFT_TEMPLATES = {
    config.BangBang: "bangBangDriftDerivative.ml.jinja",
    config.Continuous: "foo.ml.jinja",
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
    base_path = Path("./src")

    # Select the drift derivative template at runtime based on config
    GENERATE_SRCS[DRIFT_SRC] = DRIFT_TEMPLATES[
        type(checker_config.drift_derivative_curve)
    ]

    # Note: separating out generation of tokens vs general src modules since
    # the token modules need some more specific info and I would prefer to
    # be explicit about the variables we provide to template when rendering.
    template = env.get_template(TOKEN_TEMPLATE)
    for token_field, src in TOKEN_SRCS.items():
        token_config = getattr(checker_config.tokens, token_field)
        config.generate_token_src_module(
            base_path.joinpath(src),
            template,
            token_config,
        )

    for src, template_name in GENERATE_SRCS.items():
        template = env.get_template(template_name)
        config.generate_src_module(
            base_path.joinpath(src),
            template,
            checker_config,
        )


if __name__ == "__main__":
    cli()
