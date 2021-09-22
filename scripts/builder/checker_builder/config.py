"""
Configuration file logic for Checker builds
"""
import logging
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Optional
from decimal import Decimal

import yaml
from jinja2 import Environment, PackageLoader, select_autoescape
from jinja2.environment import Template
from marshmallow import Schema, ValidationError, fields
from marshmallow.decorators import post_load

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)

DEFAULT_CONFIG = Path("checker.yaml")

# ================================================================================================
# Config classes
# ================================================================================================
@dataclass
class CollateralTokenConfig:
    decimal_digits: int = 6

    @property
    def scaling_factor(self) -> int:
        return 10 ** self.decimal_digits


@dataclass
class CheckerConfig:
    collateral: CollateralTokenConfig


# ================================================================================================
# Schemas
# ================================================================================================
class CollateralTokenConfigSchema(Schema):
    decimal_digits = fields.Integer(strict=True)

    @post_load
    def make(self, data, **kwargs):
        # Extra validation logic:
        if data["decimal_digits"] < 0:
            raise ValidationError("collateral.decimal_digits must by >= 0")

        return CollateralTokenConfig(**data)


class CheckerConfigSchema(Schema):
    collateral = fields.Nested(CollateralTokenConfigSchema())

    @post_load
    def make(self, data, **kwargs):
        return CheckerConfig(**data)


# ================================================================================================
# Helpers
# ================================================================================================


def _construct_float_as_decimal(
    self, node: yaml.ScalarNode, decimal_constructor=Decimal
):
    # PyYaml constructor which converts Yaml "float" values into a Decimal directly
    # from the raw yaml string to avoid precision issues which can occur when
    # converting them to floats.
    assert node.tag == "tag:yaml.org,2002:float"
    value = node.value.lower()
    if ".inf" in value or value == ".nan" or ":" in value:
        raise ValueError(
            f"Special yaml float values such as .inf, .nan or ':' "
            f"notation are not supported in this custom yaml loader. Failed to parse: {value}"
        )
    return decimal_constructor(node.value)


Loader = yaml.SafeLoader
Loader.add_constructor("tag:yaml.org,2002:float", _construct_float_as_decimal)


def load_checker_config(path: Optional[Path] = None) -> CheckerConfig:
    if path is None:
        path = DEFAULT_CONFIG
    logger.info(f"Loading config from {path}")
    with path.open() as f:
        raw_config = yaml.load(f, Loader=Loader)
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
