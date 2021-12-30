"""
Configuration file logic for Checker builds
"""

import logging
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from os import PathLike
from pathlib import Path
from typing import Optional, Union

import yaml
from jinja2 import Environment, PackageLoader, select_autoescape
from jinja2.environment import Template
from marshmallow import Schema, ValidationError, fields
from marshmallow.decorators import post_load

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)


@dataclass(frozen=True)
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


@dataclass(frozen=True)
class ReferencedTokenConfig:
    """Config for tokens which contracts in the Checker ecosystem reference internally"""

    token_id: int
    decimal_digits: int = 6

    @property
    def scaling_factor(self) -> int:
        return 10 ** self.decimal_digits


@dataclass(frozen=True)
class IssuedTokenConfig(ReferencedTokenConfig):
    """Config for tokens which are issued by contracts in the Checker ecosystem"""

    name: str = ""
    symbol: str = ""


@dataclass(frozen=True)
class IssuedTokens:
    kit: IssuedTokenConfig
    liquidity: IssuedTokenConfig
    wctez: IssuedTokenConfig
    wtez: IssuedTokenConfig
    mock_fa2: IssuedTokenConfig


@dataclass(frozen=True)
class ReferencedTokens:
    collateral: IssuedTokenConfig
    cfmm_token: IssuedTokenConfig


@dataclass(frozen=True)
class Tokens:
    issued: IssuedTokens
    in_use: ReferencedTokens


class CollateralType(Enum):
    TEZ = "tez"
    FA2 = "fa2"


class TrackingType(Enum):
    INDEX = "index"
    TOKEN = "token"


@dataclass(frozen=True)
class Continuous:
    target_bracket: Ratio
    max_acceleration: int


@dataclass(frozen=True)
class BangBang:
    target_low_bracket: Ratio
    target_high_bracket: Ratio
    low_acceleration: int
    high_acceleration: int


@dataclass(frozen=True)
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


@dataclass(frozen=True)
class CheckerConfig:
    tracking_type: TrackingType
    collateral_type: CollateralType
    tokens: Tokens
    constants: Constants
    drift_derivative_curve: Union[BangBang, Continuous]

    def __post_init__(self) -> None:
        # Validation logic
        wtez = self.tokens.issued.wtez
        wctez = self.tokens.issued.wctez
        collateral = self.tokens.in_use.collateral
        cfmm_token = self.tokens.in_use.cfmm_token

        if self.collateral_type == CollateralType.TEZ:
            if not (
                (collateral.token_id == wtez.token_id)
                and (collateral.decimal_digits == wtez.decimal_digits)
            ):
                raise ValueError(
                    "collateral config must be identical to wtez config when collateral_type=tez"
                )
            if not (
                (cfmm_token.token_id == wctez.token_id)
                and (cfmm_token.decimal_digits == wctez.decimal_digits)
            ):
                raise ValueError(
                    "cfmm_token config must be identical to wctez config when collateral_type=tez"
                )
        elif self.collateral_type == CollateralType.FA2:
            if collateral != cfmm_token:
                raise ValueError(
                    "collateral and cfmm_token config must be identical when collateral_type=fa2"
                )

        if (self.tracking_type == TrackingType.TOKEN) and (
            self.collateral_type == CollateralType.TEZ
        ):
            raise ValueError(
                'tracking_type cannot be set to "token" when collateral_type is set to "tez"'
            )


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
            raise ValidationError("Unable to parse value to a ratio") from error
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


class IssuedTokenConfigSchema(Schema):
    token_id = BoundedIntField(lower=0, strict=True, required=True)
    decimal_digits = BoundedIntField(lower=0, strict=True)
    name = fields.String()
    symbol = fields.String()

    @post_load
    def make(self, data, **kwargs):
        return IssuedTokenConfig(**data)


class ReferencedTokenConfigSchema(Schema):
    token_id = BoundedIntField(lower=0, strict=True, required=True)
    decimal_digits = BoundedIntField(lower=0, strict=True)

    @post_load
    def make(self, data, **kwargs):
        return ReferencedTokenConfig(**data)


class IssuedTokensSchema(Schema):
    kit = fields.Nested(IssuedTokenConfigSchema(), required=True)
    liquidity = fields.Nested(IssuedTokenConfigSchema(), required=True)
    wctez = fields.Nested(IssuedTokenConfigSchema(), required=True)
    wtez = fields.Nested(IssuedTokenConfigSchema(), required=True)
    mock_fa2 = fields.Nested(IssuedTokenConfigSchema(), required=True)

    @post_load
    def make(self, data, **kwargs):
        return IssuedTokens(**data)


class ReferencedTokensSchema(Schema):
    collateral = fields.Nested(ReferencedTokenConfigSchema(), required=True)
    cfmm_token = fields.Nested(ReferencedTokenConfigSchema(), required=True)

    @post_load
    def make(self, data, **kwargs):
        return ReferencedTokens(**data)


class TokensSchema(Schema):
    issued = fields.Nested(IssuedTokensSchema(), required=True)
    in_use = fields.Nested(ReferencedTokensSchema(), required=True)

    @post_load
    def make(self, data, **kwargs):
        return Tokens(**data)


class ContinuousSchema(Schema):
    target_bracket = PositiveRatioField(required=True)
    max_acceleration = BoundedIntField(lower=1, strict=True, required=True)

    @post_load
    def make(self, data, **kwargs):
        return Continuous(**data)


class BangBangSchema(Schema):
    target_low_bracket = PositiveRatioField(required=True)
    target_high_bracket = PositiveRatioField(required=True)
    low_acceleration = BoundedIntField(lower=1, strict=True, required=True)
    high_acceleration = BoundedIntField(lower=1, strict=True, required=True)

    @post_load
    def make(self, data, **kwargs):
        return BangBang(**data)


class DriftDerivativeCurveSchema(Schema):
    curve_type = fields.String(required=True)
    parameters = fields.Dict(keys=fields.Str(), required=True)

    @post_load
    def make(self, data, **kwargs):
        if data["curve_type"] == "bang-bang":
            curve_schema = BangBangSchema()
        elif data["curve_type"] == "continuous":
            curve_schema = ContinuousSchema()
        else:
            raise ValidationError(
                f"Unknown drift derivative curve type '{data['curve_type']}'. "
                f"Valid types are: 'bang-bang', 'continuous'"
            )
        return curve_schema.load(data["parameters"])


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


class CollateralTypeField(fields.Field):
    def _deserialize(self, value, attr, data, **kwargs):
        try:
            ct = CollateralType(value)
        except ValueError as error:
            raise ValidationError("Invalid collateral type") from error
        return ct


class TrackingTypeField(fields.Field):
    def _deserialize(self, value, attr, data, **kwargs):
        try:
            tt = TrackingType(value)
        except ValueError as error:
            raise ValidationError("Invalid tracking type") from error
        return tt


class CheckerConfigSchema(Schema):
    tracking_type = TrackingTypeField(required=True)
    collateral_type = CollateralTypeField(required=True)
    tokens = fields.Nested(TokensSchema(), required=True)
    constants = fields.Nested(ConstantsSchema(), required=True)
    drift_derivative_curve = fields.Nested(DriftDerivativeCurveSchema(), required=True)

    @post_load
    def make(self, data, **kwargs):
        return CheckerConfig(**data)


# ================================================================================================
# Helpers
# ================================================================================================
# Default yaml Loader for this package
Loader = yaml.SafeLoader


class CheckerRepo:
    """File paths for key items in the Checker repo"""

    def __init__(self, path: PathLike):
        self.root = Path(path)

    @property
    def default_config(self) -> Path:
        return self.root.joinpath("checker.yaml")

    @property
    def src(self) -> Path:
        return self.root.joinpath("src")

    @property
    def input_config(self) -> Path:
        return self.src.joinpath("_input_checker.yaml")

    @property
    def generated_ligo(self) -> Path:
        return self.root.joinpath("generated").joinpath("ligo")

    @property
    def generated_michelson(self) -> Path:
        return self.root.joinpath("generated").joinpath("michelson")

    @property
    def checker_contract(self) -> Path:
        return self.generated_michelson.joinpath("main.tz")

    @property
    def checker_functions(self) -> Path:
        return self.generated_michelson.joinpath("functions.json")

    @property
    def mock_fa2_contract(self) -> Path:
        return self.generated_michelson.joinpath("mockFA2Main.tz")

    @property
    def mock_fa2_metadata(self) -> Path:
        return self.generated_michelson.joinpath("mock_fa2_metadata.json")

    @property
    def wtez_contract(self) -> Path:
        return self.generated_michelson.joinpath("wtezMain.tz")

    @property
    def wtez_metadata(self) -> Path:
        return self.generated_michelson.joinpath("wtez_metadata.json")

    @property
    def wctez_contract(self) -> Path:
        return self.generated_michelson.joinpath("wctezMain.tz")

    @property
    def wctez_metadata(self) -> Path:
        return self.generated_michelson.joinpath("wctez_metadata.json")

    @property
    def mock_oracle_contract(self) -> Path:
        return self.root.joinpath("util").joinpath("mock_oracle.tz")

    @property
    def mock_cfmm_oracle_contract(self) -> Path:
        return self.root.joinpath("util").joinpath("mock_cfmm_oracle.tz")

    @property
    def ctez(self) -> Path:
        return self.root.joinpath("vendor").joinpath("ctez")


def load_checker_config(path: Optional[Path] = None) -> CheckerConfig:
    """Loads a checker yaml configuration file

    Args:
        path: The path to the config file. If none is specified a default is used.

    Returns:
        The parsed CheckerConfig
    """
    if path is None:
        path = CheckerRepo(".").default_config
    logger.info(f"Loading config from {path}")
    with path.open() as f:
        raw_config = yaml.load(f, Loader=Loader)
    return CheckerConfigSchema().load(raw_config)


def load_input_config(repo: Optional[CheckerRepo] = None) -> CheckerConfig:
    """Loads the checker configuration file used as the input for code generation

    Args:
        repo_root: The Checker repository root directory. Defaults to the current working directory.

    Returns:
        The parsed CheckerConfig

    Raises:
        FileNotFoundError: If no input config is found.
    """
    if repo is None:
        repo = CheckerRepo(".")
    if not repo.input_config.exists():
        raise FileNotFoundError(
            f"No configuration file found at {repo.input_config}. This file should be automatically "
            f"generated as a part of the code generation process."
        )
    with repo.input_config.open() as f:
        raw_config = yaml.load(f, Loader=Loader)
    return CheckerConfigSchema().load(raw_config)


def load_template_env() -> Environment:
    """Gets the default jinja2 environment"""
    return Environment(
        loader=PackageLoader("checker_builder"), autoescape=select_autoescape()
    )


def generate_token_src_module(
    module: Path, template: Template, token_config: ReferencedTokenConfig
) -> None:
    """Generates a token source code module using the provided template and configuration

    Provides the following variables to the template:
        * token_config (ReferencedTokenConfig)
        * module_name (str)
    """
    logger.info(
        f"Rendering token src module template '{template.name}' using provided config"
    )
    module_name = module.name.split(".")[0]
    rendered = template.render(module_name=module_name, token_config=token_config)
    logger.info(f"Writing rendered module at {module}")
    with module.open("w") as f:
        f.write(rendered)


def generate_src_module(
    module: Path, template: Template, config: CheckerConfig
) -> None:
    """Generates a source code module using the provided template and configuration

    Provides the following variables to the template:
        * config (CheckerConfig)
    """
    logger.info(
        f"Rendering src module template '{template.name}' using provided config"
    )
    rendered = template.render(config=config)
    logger.info(f"Writing rendered module at {module}")
    with module.open("w") as f:
        f.write(rendered)
