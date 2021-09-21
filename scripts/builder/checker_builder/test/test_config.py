import pytest
from marshmallow import ValidationError


def test_collateral_config_decimal_digits():
    from checker_builder.config import CollateralTokenConfig

    collateral_config = CollateralTokenConfig(scaling_factor=12345)
    assert collateral_config.decimal_digits == 4


def test_collateral_config_schema_zero_scaling_factor():
    from checker_builder.config import CollateralTokenConfigSchema

    data = {"scaling_factor": 0}
    with pytest.raises(ValidationError):
        CollateralTokenConfigSchema().load(data)


def test_collateral_config_schema_negative_scaling_factor():
    from checker_builder.config import CollateralTokenConfigSchema

    data = {"scaling_factor": -1}
    with pytest.raises(ValidationError):
        CollateralTokenConfigSchema().load(data)
