"""
Configuration file logic for Checker builds
"""

from decimal import Decimal
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import yaml
from jinja2 import Environment, PackageLoader, select_autoescape
from jinja2.environment import Template
from marshmallow import Schema, ValidationError, fields
from marshmallow.decorators import post_load

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)

DEFAULT_CONFIG = Path("checker.yaml")


@dataclass
class Ratio:
    # Numerator
    num: int
    # Denominator
    den: int


def ratio_from_str(ratio_str: str) -> Ratio:
    """
    Args:
        ratio_str: The raw string representing the ratio

    Returns:
        The parsed ratio

    Raises:
        ValueError: If the input string cannot be parsed
    """
    parts = ratio_str.strip().split("/", maxsplit=1)
    if len(parts) != 2:
        raise ValueError(f"Unable to parse ratio from string '{ratio_str}'. ")
    num_str, den_str = parts
    num = int(num_str.strip())
    den = int(den_str.strip())
    return Ratio(num, den)


# ================================================================================================
# Config classes
# ================================================================================================
@dataclass
class TokenConfig:
    token_id: int
    decimal_digits: int = 6
    name: str = ""
    symbol: str = ""

    @property
    def scaling_factor(self) -> int:
        return 10 ** self.decimal_digits


@dataclass
class Tokens:
    collateral: TokenConfig
    kit: TokenConfig
    liquidity: TokenConfig


@dataclass
class Constants:
    """Checker system constants"""

    auction_decay_rate: Ratio
    bid_improvement_factor: Ratio
    burrow_fee_percentage: Ratio
    cfmm_fee: Ratio
    creation_deposit: int
    fliquidation: Ratio
    fminting: Ratio
    imbalance_limit: Ratio
    imbalance_scaling_factor: Ratio
    liquidation_penalty: Ratio
    liquidation_reward_percentage: Ratio
    max_bid_interval_in_blocks: int
    max_bid_interval_in_seconds: int
    max_liquidation_queue_height: int
    max_lot_size: int
    min_lot_auction_queue_fraction: Ratio
    number_of_slices_to_process: int
    protected_index_inverse_epsilon: int
    touch_high_reward: Ratio
    touch_low_reward: Ratio
    touch_reward_low_bracket: int


@dataclass
class CheckerConfig:
    tokens: Tokens
    constants: Constants


# ================================================================================================
# Schemas
# ================================================================================================
class BoundedIntField(fields.Integer):
    def __init__(self, *, lower=None, upper=None, **kwargs):
        self.lower_bound = lower
        self.upper_bound = upper
        super().__init__(**kwargs)

    def _deserialize(self, value, attr, data, **kwargs):
        value = super()._deserialize(value, attr, data, **kwargs)
        if self.lower_bound is not None and value < self.lower_bound:
            raise ValidationError(
                f"Value is less than required lower bound of: {self.lower_bound}"
            )
        if self.upper_bound is not None and value > self.upper_bound:
            raise ValidationError(
                f"Value is greater than required upper bound of: {self.upper_bound}"
            )
        return value


class RatioField(fields.Field):
    def _serialize(self, value: Ratio, attr, obj, **kwargs):
        return f"{value.num}/{value.den}"

    def _deserialize(self, value, attr, data, **kwargs):
        try:
            ratio = ratio_from_str(value)
        except ValueError as error:
            raise ValidationError from error
        if ratio.den <= 0:
            raise ValidationError(
                f"Provided ratio had a non-positive denominator: {value}"
            )
        return ratio


class PositiveRatioField(RatioField):
    def _deserialize(self, value, attr, data, **kwargs):
        ratio = super()._deserialize(value, attr, data, **kwargs)
        if (
            (ratio.num < 0 and ratio.den >= 0)
            or (ratio.num >= 0 and ratio.den < 0)
            or (ratio.num == 0)
        ):
            raise ValidationError(f"The provided ratio was not positive: {ratio}")
        return ratio


class TokenConfigSchema(Schema):
    token_id = BoundedIntField(lower=0, strict=True, required=True)
    decimal_digits = BoundedIntField(lower=0, strict=True)
    name = fields.String()
    symbol = fields.String()

    @post_load
    def make(self, data, **kwargs):
        return TokenConfig(**data)


class TokensSchema(Schema):
    collateral = fields.Nested(TokenConfigSchema(), required=True)
    kit = fields.Nested(TokenConfigSchema(), required=True)
    liquidity = fields.Nested(TokenConfigSchema(), required=True)

    @post_load
    def make(self, data, **kwargs):
        # FIXME: Once we add a switch for indicating whether we are using
        # the tez wrapper for collateral we can add logic here checking
        # that all of the token ids are unique.
        return Tokens(**data)


class ConstantsSchema(Schema):

    creation_deposit = BoundedIntField(lower=1, strict=True, required=True)
    # TODO: double check that fliquidation and fminting *must* be greater than 1
    fliquidation = PositiveRatioField(required=True)  # > 1
    fminting = PositiveRatioField(required=True)  # > 1, and should be > fliquidation

    burrow_fee_percentage = PositiveRatioField(required=True)  # positive
    cfmm_fee = PositiveRatioField(required=True)  # positive
    imbalance_limit = PositiveRatioField(required=True)  # positive
    imbalance_scaling_factor = PositiveRatioField(required=True)  # positive
    liquidation_penalty = PositiveRatioField(required=True)  # positive
    liquidation_reward_percentage = PositiveRatioField(required=True)  # positive
    protected_index_inverse_epsilon = BoundedIntField(
        lower=1, strict=True, required=True
    )

    auction_decay_rate = PositiveRatioField(required=True)  # positive
    bid_improvement_factor = PositiveRatioField(required=True)  # positive
    max_bid_interval_in_blocks = BoundedIntField(lower=1, strict=True, required=True)
    max_bid_interval_in_seconds = BoundedIntField(lower=1, strict=True, required=True)
    max_liquidation_queue_height = BoundedIntField(lower=1, strict=True, required=True)
    max_lot_size = BoundedIntField(lower=1, strict=True, required=True)
    min_lot_auction_queue_fraction = PositiveRatioField(required=True)  # positive

    number_of_slices_to_process = BoundedIntField(lower=1, strict=True, required=True)
    touch_high_reward = PositiveRatioField(required=True)  # positive
    touch_low_reward = PositiveRatioField(required=True)  # positive
    touch_reward_low_bracket = BoundedIntField(lower=1, strict=True, required=True)

    @post_load
    def make(self, data, **kwargs):
        # Note: This comparison is only precise to the (fixed) precision used by Decimal, but since this
        # is a "soft" check this should be acceptable.
        fminting = Decimal(data["fminting"].num) / Decimal(data["fminting"].den)
        fliquidation = Decimal(data["fliquidation"].num) / Decimal(
            data["fliquidation"].den
        )
        if fminting <= fliquidation:
            raise ValidationError("fminting must be > fliquidation")
        if fminting <= 1 or fliquidation <= 1:
            raise ValidationError(
                "Both fminting and fliquidation must be greater than 1"
            )
        return Constants(**data)


class CheckerConfigSchema(Schema):
    tokens = fields.Nested(TokensSchema(), required=True)
    constants = fields.Nested(ConstantsSchema(), required=True)

    @post_load
    def make(self, data, **kwargs):
        return CheckerConfig(**data)


# ================================================================================================
# Helpers
# ================================================================================================
# Default yaml Loader for this package
Loader = yaml.SafeLoader


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
