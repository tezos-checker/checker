from io import StringIO

import pytest
import yaml
from marshmallow import ValidationError

DECIMAL_YAML = """
some_int: 1
some_str: foo
some_ratio: 1/42
"""


def test_serialize_ratio():
    from checker_builder.config import Ratio, RatioField

    input = Ratio(1, 3)
    assert RatioField().serialize("ratio", obj={"ratio": input}) == "1/3"


def test_deserialize_ratio():
    from checker_builder.config import Ratio, RatioField

    assert RatioField().deserialize("1/9") == Ratio(1, 9)


def test_deserialize_ratio_zero_den_fails():
    from checker_builder.config import RatioField

    with pytest.raises(ValidationError):
        RatioField().deserialize("1/0")


def test_serialiation_roundtrip_ratio():
    from checker_builder.config import Ratio, RatioField

    input = Ratio(-3, 997)
    field = RatioField()
    assert field.deserialize(field.serialize("ratio", obj={"ratio": input})) == input


def test_deserialize_positive_ratio():
    from checker_builder.config import PositiveRatioField, Ratio

    assert PositiveRatioField().deserialize("-1/-9") == Ratio(-1, -9)
    assert PositiveRatioField().deserialize("1/9") == Ratio(1, 9)


def test_deserialize_positive_ratio_negative_fails():
    from checker_builder.config import PositiveRatioField

    field = PositiveRatioField()

    with pytest.raises(ValidationError):
        field.deserialize("-1/1")
    with pytest.raises(ValidationError):
        field.deserialize("1/-1")


def test_deserialize_positive_ratio_zero_fails():
    from checker_builder.config import PositiveRatioField

    with pytest.raises(ValidationError):
        PositiveRatioField().deserialize("0/1")


def test_ratio_from_str():
    from checker_builder.config import Ratio, ratio_from_str

    assert ratio_from_str(" 1 /  2 ") == Ratio(1, 2)


def test_ratio_from_str_only_numerator_fails():
    from checker_builder.config import ratio_from_str

    with pytest.raises(ValueError):
        ratio_from_str("3")


def test_ratio_from_str_extra_slash_fails():
    from checker_builder.config import ratio_from_str

    with pytest.raises(ValueError):
        ratio_from_str("3/2/4")


def test_collateral_config_decimal_digits():
    from checker_builder.config import CollateralTokenConfig

    collateral_config = CollateralTokenConfig(decimal_digits=3)
    assert collateral_config.scaling_factor == 1000


def test_bounded_int_field_lower_inclusive():
    from checker_builder.config import BoundedIntField

    assert BoundedIntField(lower=-1).deserialize("-1") == -1


def test_bounded_int_field_upper_inclusive():
    from checker_builder.config import BoundedIntField

    assert BoundedIntField(upper=-1).deserialize("-1") == -1


def test_bounded_int_field_lower():
    from checker_builder.config import BoundedIntField

    with pytest.raises(ValidationError):
        BoundedIntField(lower=-1).deserialize("-2")


def test_bounded_int_field_upper():
    from checker_builder.config import BoundedIntField

    with pytest.raises(ValidationError):
        BoundedIntField(upper=-1).deserialize("0")


def test_bounded_int_field_lower_and_upper():
    from checker_builder.config import BoundedIntField

    with pytest.raises(ValidationError):
        BoundedIntField(lower=-1, upper=3).deserialize("-2")
    with pytest.raises(ValidationError):
        BoundedIntField(lower=-1, upper=3).deserialize("4")


def test_yaml_loader_loads_ratio_as_str():
    from checker_builder.config import Loader

    f = StringIO(DECIMAL_YAML)
    data = yaml.load(f, Loader=Loader)

    assert type(data["some_ratio"]) == str
