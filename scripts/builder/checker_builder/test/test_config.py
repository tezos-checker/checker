from io import StringIO
from decimal import Decimal
import pytest
from marshmallow import ValidationError
import yaml

DECIMAL_YAML = """
some_int: 1
some_str: foo
some_decimal: 1.234
nested:
    another_decimal: .025
"""


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


def test_load_yaml_to_decimal():
    from checker_builder.config import Loader

    f = StringIO(DECIMAL_YAML)
    data = yaml.load(f, Loader=Loader)

    assert type(data["some_int"]) == int
    assert type(data["some_str"]) == str
    assert type(data["some_decimal"]) == Decimal
    assert type(data["nested"]["another_decimal"]) == Decimal
    assert data["some_decimal"] == Decimal("1.234")
    assert data["nested"]["another_decimal"] == Decimal(".025")
