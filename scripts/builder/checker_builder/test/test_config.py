import pytest
from marshmallow import ValidationError


def test_collateral_config_decimal_digits():
    from checker_builder.config import CollateralTokenConfig

    collateral_config = CollateralTokenConfig(decimal_digits=3)
    assert collateral_config.scaling_factor == 1000


def test_collateral_config_schema_positive_scaling_factor():
    from checker_builder.config import (
        CollateralTokenConfig,
        CollateralTokenConfigSchema,
    )

    data = {"decimal_digits": 1}
    assert CollateralTokenConfigSchema().load(data) == CollateralTokenConfig(
        decimal_digits=1
    )


def test_collateral_config_schema_zero_scaling_factor_fails():
    from checker_builder.config import (
        CollateralTokenConfig,
        CollateralTokenConfigSchema,
    )

    data = {"decimal_digits": 0}
    assert CollateralTokenConfigSchema().load(data) == CollateralTokenConfig(
        decimal_digits=0
    )


def test_collateral_config_schema_negative_scaling_factor_fails():
    from checker_builder.config import CollateralTokenConfigSchema

    data = {"decimal_digits": -1}
    with pytest.raises(ValidationError):
        CollateralTokenConfigSchema().load(data)
