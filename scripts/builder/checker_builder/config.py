"""
Configuration file schemas for Checker
"""
import logging
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import yaml
from jinja2 import Environment, PackageLoader, select_autoescape
from jinja2.environment import Template
from marshmallow import Schema, fields
from marshmallow.decorators import post_load

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)

DEFAULT_CONFIG = Path("checker.yaml")

# TODO: Additional support for setting certain parameters at build time can be implemented
# using:
# def commented(comment: str, default=None) -> Field:
#     """Creates a dataclass field with a comment in its metadata"""
#     return field(default=default, metadata={"comment": comment})
# @dataclass
# class SystemParameters:
#     """Checker system parameters"""

#     fminting: Decimal = commented(
#         "Dimensionless. Factor used for setting the minting limit.",
#         default=(Decimal(21) / Decimal(10)),
#     )


@dataclass
class CollateralTokenConfig:
    scaling_factor: int = 1_000_000

    @property
    def decimal_digits(self) -> int:
        return math.floor(math.log10(self.scaling_factor))


@dataclass
class CheckerConfig:
    collateral: CollateralTokenConfig


# ================================================================================================
# Schemas
# ================================================================================================
class CollateralTokenConfigSchema(Schema):
    scaling_factor = fields.Integer(strict=True)

    @post_load
    def make(self, data, **kwargs):
        return CollateralTokenConfig(**data)


class CheckerConfigSchema(Schema):
    collateral = fields.Nested(CollateralTokenConfigSchema())

    @post_load
    def make(self, data, **kwargs):
        return CheckerConfig(**data)


def load_checker_config(path: Optional[Path] = None) -> CheckerConfig:
    if path is None:
        path = DEFAULT_CONFIG
    logger.info(f"Loading config from {path}")
    with path.open() as f:
        raw_config = yaml.load(f, Loader=yaml.SafeLoader)
    return CheckerConfigSchema().load(raw_config)


def load_template_env() -> Environment:
    """Gets the default jinja2 environment"""
    return Environment(
        loader=PackageLoader("checker_builder"), autoescape=select_autoescape()
    )


def generate_src_module(module: Path, template: Template, config: CheckerConfig):
    """Generates a source code module using the provided template and configuration"""
    logger.info(
        f"Rendering src module template '{template.name}' using provided config"
    )
    rendered = template.render(config=config)
    logger.info(f"Writing rendered module at {module}")
    with module.open("w") as f:
        f.write(rendered)
