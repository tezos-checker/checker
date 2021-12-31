import shutil
from pathlib import Path

import click

from checker_tools.builder import config

# Mapping of generated src modules to their templates
GENERATE_SRCS = {
    "constants.ml": "constants.ml.jinja",
    "burrowOrigination.ml": "burrowOrigination.ml.jinja",
    "tokenMetadata.ml": "tokenMetadata.ml.jinja",
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

GET_ORACLE_ENTRYPOINT_SRC = "getOracleEntrypoint.ml"
GET_ORACLE_ENTRYPOINT_TEMPLATES = {
    config.TrackingType.INDEX: "indexGetOracleEntrypoint.ml.jinja",
    config.TrackingType.TOKEN: "tokenGetOracleEntrypoint.ml.jinja",
}

TARGET_CALCULATION_SRC = "targetCalculation.ml"
TARGET_CALCULATION_TEMPLATES = {
    config.TrackingType.INDEX: "indexTargetCalculation.ml.jinja",
    config.TrackingType.TOKEN: "tokenTargetCalculation.ml.jinja",
}


@click.group()
def cli():
    """Checker build utilities"""
    pass


@cli.command()
@click.option(
    "--out",
    required=True,
    type=click.Path(file_okay=False),
    help="The directory to write the generated modules to. Created if it does not exist already.",
)
def generate(out: str):
    """Run code generation"""
    repo = config.CheckerRepo(".")
    config_path = repo.default_config
    out = Path(out)

    if not out.exists():
        out.mkdir()

    checker_config = config.load_checker_config(config_path)
    env = config.load_template_env()

    # Select the drift derivative template at runtime based on config
    GENERATE_SRCS[DRIFT_SRC] = DRIFT_TEMPLATES[type(checker_config.drift_derivative_curve)]

    # Select the price calculation module at runtime based on config
    GENERATE_SRCS[PRICE_SRC] = PRICE_TEMPLATES[checker_config.collateral_type]

    # Select the oracle configuration at runtime based on config
    GENERATE_SRCS[GET_ORACLE_ENTRYPOINT_SRC] = GET_ORACLE_ENTRYPOINT_TEMPLATES[
        checker_config.tracking_type
    ]

    # Select the target calculation at runtime based on config
    GENERATE_SRCS[TARGET_CALCULATION_SRC] = TARGET_CALCULATION_TEMPLATES[
        checker_config.tracking_type
    ]

    # Note: separating out generation of tokens vs general src modules since
    # the token modules need some more specific info and I would prefer to
    # be explicit about the variables we provide to template when rendering.
    template = env.get_template(TOKEN_TEMPLATE)
    for token_type, token_type_srcs in TOKEN_SRCS.items():
        for token_field, src in token_type_srcs.items():
            token_config = getattr(getattr(checker_config.tokens, token_type), token_field)
            config.generate_token_src_module(
                out.joinpath(src),
                template,
                token_config,
            )

    for src, template_name in GENERATE_SRCS.items():
        template = env.get_template(template_name)
        config.generate_src_module(
            out.joinpath(src),
            template,
            checker_config,
        )

    # Store the input configuration file for downstream processes to use (e.g. for deployment)
    shutil.copy(config_path, repo.input_config)


if __name__ == "__main__":
    cli()
