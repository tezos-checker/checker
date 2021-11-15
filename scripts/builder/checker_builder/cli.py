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
# Mappings of token config fields to their corresponding src modules
TOKEN_SRCS = {
    "issued": {
        "kit": "kit.ml",
        "liquidity": "lqt.ml",
    },
    "in_use": {
        "collateral": "tok.ml",
        "cfmm_token": "ctok.ml",
    },
}

DRIFT_SRC = "driftDerivative.ml"
DRIFT_TEMPLATES = {
    config.BangBang: "bangBangDriftDerivative.ml.jinja",
    config.Continuous: "continuousDriftDerivative.ml.jinja",
}

PRICE_SRC = "price.ml"
PRICE_TEMPLATES = {
    config.CollateralType.TEZ: "tezPrice.ml.jinja",
    config.CollateralType.FA2: "fa2Price.ml.jinja",
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

    # Select the price calculation module at runtime based on config
    GENERATE_SRCS[PRICE_SRC] = PRICE_TEMPLATES[checker_config.collateral_type]

    # Note: separating out generation of tokens vs general src modules since
    # the token modules need some more specific info and I would prefer to
    # be explicit about the variables we provide to template when rendering.
    template = env.get_template(TOKEN_TEMPLATE)
    for token_type, token_type_srcs in TOKEN_SRCS.items():
        for token_field, src in token_type_srcs.items():
            token_config = getattr(
                getattr(checker_config.tokens, token_type), token_field
            )
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

    # FIXME: Generate token metadata module here


if __name__ == "__main__":
    cli()
