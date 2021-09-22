"""
Configuration file logic for Checker builds
"""
import logging
from dataclasses import dataclass, Field, field
from pathlib import Path
from typing import Optional
from decimal import Decimal, getcontext, Rounded

import yaml
from jinja2 import Environment, PackageLoader, select_autoescape
from jinja2.environment import Template
from marshmallow import Schema, ValidationError, fields
from marshmallow.decorators import post_load

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)

DEFAULT_CONFIG = Path("checker.yaml")

# Set global decimal context to raise exception on rounding for extra safety.
# Note: This only affects cases where we are operating on the Decimal value -
# the Decimal constructor does not truncate its input.
DECIMAL_PRECISION = 28
decimal_context = getcontext()
decimal_context.prec = DECIMAL_PRECISION
decimal_context.traps[Rounded] = True


def commented(comment: str, default=None) -> Field:
    """Creates a dataclass field with a comment in its metadata"""
    return field(default=default, metadata={"comment": comment})


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
class Constants:
    """Checker system constants"""

    # Note: Any Decimal (ratio) values should be specified using literals only to
    # avoid issues with precision.
    fminting: Decimal = Decimal("2.1")
    fliquidation: Decimal = Decimal("1.9")
    creation_deposit: int = 1
    burrow_fee_percentage: Decimal = Decimal("0.005")
    imbalance_scaling_factor: Decimal = Decimal("0.75")
    imbalance_limit: Decimal = Decimal("0.05")
    liquidation_reward_percentage: Decimal = Decimal("0.001")
    cfmm_fee: Decimal = Decimal("0.002")
    protected_index_inverse_epsilon: int = 120_000
    max_lot_size: int = 10_000_000_000
    min_lot_auction_queue_fraction: Decimal = Decimal("0.05")
    liquidation_penalty: Decimal = Decimal("0.1")
    target_low_bracket: Decimal = Decimal("0.005")
    target_high_bracket: Decimal = Decimal("0.05")
    low_positive_acceleration: int = 247111
    low_negative_acceleration: int = -247111
    high_positive_acceleration: int = 1235555
    high_negative_acceleration: int = -1235555
    auction_decay_rate: Decimal = Decimal("0.01") / Decimal("60")
    max_bid_interval_in_seconds: int = 1200
    max_bid_interval_in_blocks: int = 20
    bid_improvement_factor: Decimal = Decimal("0.0033")
    touch_reward_low_bracket: int = 600
    touch_low_reward: Decimal = Decimal("0.1") / Decimal("60")
    # FIXME: Hard to specify this ratio in yaml file
    touch_high_reward: Decimal = Decimal("1") / Decimal("60")
    number_of_slices_to_process: int = 5
    max_liquidation_queue_height: int = 12


@dataclass
class CheckerConfig:
    collateral: CollateralTokenConfig
    constants: Constants


# ================================================================================================
# Schemas
# ================================================================================================
class NonNegativeInt(fields.Integer):
    def _deserialize(self, value, attr, data, **kwargs):
        value = super()._deserialize(value, attr, data, **kwargs)
        if value < 0:
            raise ValidationError(f"Value must be non-negative: {value}")
        return value


class PositiveInt(fields.Integer):
    def _deserialize(self, value, attr, data, **kwargs):
        value = super()._deserialize(value, attr, data, **kwargs)
        if value <= 0:
            raise ValidationError(f"Value must be positive: {value}")
        return value


class PositiveDecimal(fields.Decimal):
    def _deserialize(self, value, attr, data, **kwargs):
        value = super()._deserialize(value, attr, data, **kwargs)
        if value <= 0:
            raise ValidationError(f"Value must be positive: {value}")
        if not value.is_finite():
            raise ValidationError(f"Value must be finite: {value}")
        return value


class CollateralTokenConfigSchema(Schema):
    decimal_digits = NonNegativeInt(strict=True)

    @post_load
    def make(self, data, **kwargs):
        return CollateralTokenConfig(**data)


class ConstantsSchema(Schema):
    fminting = PositiveDecimal(allow_nan=False)
    fliquidation = PositiveDecimal(allow_nan=False)

    @post_load
    def make(self, data, **kwargs):
        return Constants(**data)


class CheckerConfigSchema(Schema):
    collateral = fields.Nested(CollateralTokenConfigSchema())
    constants = fields.Nested(ConstantsSchema())

    @post_load
    def make(self, data, **kwargs):
        return CheckerConfig(**data)


# ================================================================================================
# Helpers
# ================================================================================================


def num(x: Decimal) -> int:
    """Gets the numerator of a decimal's ratio representation"""
    return x.as_integer_ratio()[0]


def den(x: Decimal) -> int:
    """Gets the denominator of a decimal's ratio representation"""
    return x.as_integer_ratio()[1]


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
    rendered = template.render(config=config, num=num, den=den)
    logger.info(f"Writing rendered module at {module}")
    with module.open("w") as f:
        f.write(rendered)
