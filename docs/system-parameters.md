# System Parameters

A operational description of Checker's internal parameters, and operations on
them. NOTE: here we focus primarily on the specifics of the calculations; the
meaning of the concepts is explained better in the
[original spec](https://hackmd.io/teMO2x9PRRy1iTBtrSMBvA).

## State

* `q`: of type (1 / kit).
* `index`: of type tez.
* `protected_index`:  of type tez.
* `target`: TODO: said dimensionless, but I think tez/kit? Hmmm. I have to re-check the units of measure.
* `drift_derivative`
* `drift`
* `outstanding_kit`: approximation of the total amount of kit that would be currently required to close all burrows.
* `circulating_kit`: approximation of the total amount of kit that is currently in circulation.
* `last_touched`: the last time the parameters were touched.

And two additional indices, one used in the calculation of _burrowing fees_ and
one in the calculation of the _imbalance adjustment_:
* `burrow_fee_index`
* `imbalance_index`

QQ: We discussed the possibility of storing `1/q` instead of `q` in Checker's state. Would that be beneficial in any way?

QQ: How much can `outstanding_kit` and `circulating_kit` deviate from reality? How big an issue can that be?

## Initialization

We currently initialize checker with the following parameters:

```
q = 1
index = 1xtz
protected_index = 1xtz
target = 1
drift = 0
drift_derivative = 0
outstanding_kit = 0mukit
circulating_kit = 0mukit
last_touched = now
burrow_fee_index = 1
imbalance_index  = 1
```

## Price API

```
tz_minting     = max index protected_index (in tez)
tz_liquidation = min index protected_index (in tez)
```
To calculate the current prices in (tez/kit), we multiply with the current quantity:
```
minting_price     = q * tz_minting
liquidation_price = q * tz_liquidation
```

### Note
The definition of `tz_minting` and `tz_liquidation` implies that at any given
moment, `tz_minting >= tz_liquidation > 0`. Combined with
`fminting > fliquidation`, we have that
```
tz_minting * fminting > tz_liquidation * fliquidation
```
which is useful in liquidation logic (see burrow-state-liquidations.md).

## Adjustment index

The adjustment index, as required by burrowing logic, can be calculated from
the system parameters as the product of the burrow fee index and the imbalance
index:
```
adjustment_index = burrow_fee_index * imbalance_index
```

## Touching

Touching the system parameters has the effect of updating all aforementioned
fields, and calculating the burrowing fees that need to be accrued to the cfmm
sub-contract. This is done under the assumption that we have available the
current time `now`, the current index `index_now` (calculated by the
medianizer), and the current price of kit in tez `kit_in_tez_now` (calculated
by the cfmm sub-contract). In fact, the cfmm sub-contract gives us the one
calculated at the end of the last block, to make manipulation a little harder.
We update each field:

### `last_touched`
Update the timestamp from the last time it was touched to now
```
new_last_touched = now
```
### `index`
Update the index from the last time the parameters were touched to the current one
```
new_index = index_now
```
### `protected_index`
Update the protected index, by multiplying it with a bounded factor:
```
new_protected_index = old_protected_index * clamp (current_index / protected_index, low, high)
```
where `low` and `high` depend on how much time has passed since the last time
the parameters were touched, effectively limiting how fast `protected_index`
can change:
```
low  = exp ( epsilon * (now - last_touched))
high = exp (-epsilon * (now - last_touched))
```
**NOTE**: `exp (x) = 1 + x` here; we expect the contract to be touched rather
frequently, which keeps the exponent rather small, which makes this a good
approximation of `exp`.

### `drift_derivative`
For the calculation of the derivative of `drift`, `drift_derivative`, we only
use the last-observed target (TODO: show how we get from the original formula
with the logarithms to this?) We calculate as follows:
```
new_drift_derivative =
  -0.0005 / (secs_in_a_day ^ 2) , if                        target <= exp (-high_bracket)
  -0.0001 / (secs_in_a_day ^ 2) , if exp (-high_bracket) <  target <= exp (-low_bracket)
   0                            , if exp (-low_bracket)  <  target <  exp ( low_bracket)
   0.0001 / (secs_in_a_day ^ 2) , if exp ( low_bracket)  <= target <  exp ( high_bracket)
   0.0005 / (secs_in_a_day ^ 2) , if exp ( high_bracket) <= target
```

TODO: Eventually we should pre-calculate all constant things (e.g. `exp (-low_bracket)`).

### `drift`
For the calculation of the current drift, we use use the following formula:
```
new_drift = old_drift + (1/2) * (old_drift_derivative + new_drift_derivative) * (now - last_touched)
```

 ### `q`
For the calculation of the current quantity, we use use the following formula:
```
new_q = old_q
      * exp (
          (old_drift + (1/6) * ((2 * old_drift_derivative) + new_drift_derivative) * (now - last_touched))
          * (now - last_touched)
        )
```

**NOTE**: `exp (x) = 1 + x` here; TODO: not sure if the exponent is small
enough for this to be a good approximation.

### `target`
```
new_target = new_q * (new_index / kit_in_tez_now)
```

### `burrow_fee_index`

The burrow fee index is updated linearly on the number of seconds that have
passed since the last time the parameters were touched.
```
new_burrow_fee_index = old_burrow_fee_index
                     * (1 + burrow_fee_rate * (now - last_touched) / seconds_in_a_year)
```

### `imbalance_index`

The imbalance index is also updated linearly on the number of seconds that have
passed since the last time the parameters were touched
```
new_imbalance_index = old_imbalance_index
                    * (1 + imbalance_rate * (now - last_touched) / seconds_in_a_year)
```
but `imbalance_rate` varies, depending on the difference between
`old_outstanding_kit` and `old_circulating_kit`:
```
imbalance_rate =
  clamp
    ( imbalance_scaling_factor * (circulating - outstanding) / circulating,
      -imbalance_limit,
      +imbalance_limit
    )
```
or, equivalently:
```
imbalance_rate =
  min (imbalance_scaling_factor * (circulating - outstanding) / circulating, +imbalance_limit), if circulating >= outstanding
  max (imbalance_scaling_factor * (circulating - outstanding) / circulating, -imbalance_limit), if circulating < outstanding
```
And in the edge cases the `imbalance_rate` is calculated as follows:
* if `old_circulating_kit = 0` and `old_outstanding_kit = 0` then `imbalance_rate = 0`.
* if `old_circulating_kit = 0` and `old_outstanding_kit > 0` then `imbalance_rate = -imbalance_limit`.
  (the outstanding kit is _infinitely_ greater than the circulating kit, so the rate is saturated).

###  Intermediate `outstanding_kit`
In order to compute the updates for the two remaining fields (`outstanding_kit`
and `circulating_kit`), we first need to calculate the current amount of kit
outstanding, taking into account the accrued burrowing fee, thus
```
outstanding_with_fees = old_outstanding_kit * (new_burrow_fee_index / old_burrow_fee_index)
```

### Accrual to cfmm
The accrued burrowing fees are to be given to the cfmm sub-contract. The total
amount we easily compute as
```
accrual_to_cfmm = outstanding_with_fees - old_outstanding
```

### `outstanding_kit`
To obtain the updated `outstanding_kit`, we need to account for both the
accrued burrowing fees, and the imbalance adjustment
```
new_outstanding_kit = old_outstanding_kit
                    * (new_burrow_fee_index / old_burrow_fee_index)
                    * (new_imbalance_index / old_imbalance_index)
```
or equivalently
```
new_outstanding_kit = outstanding_with_fees * (new_imbalance_index / old_imbalance_index)
```

### `circulating_kit`
Finally, to obtain the up-to-date `circulating_kit`, we just need to record the
new kit in circulation, that is, `accrual_to_cfmm`:
```
new_circulating_kit = old_circulating_kit + accrual_to_cfmm
```

**NOTE**: If the current timestamp is identical to that stored in the
parameters, we do not perform any of the above.

## Misc

* `seconds_in_a_year = 31556952 (= (365 + 1/4 - 1/100 + 1/400) days * 24 * 60 * 60)`
* `seconds_in_a_day  = 86400 (= 24 * 60 * 60)`
* `low_bracket  = 0.005`
* `high_bracket = 0.05`
* `imbalance_scaling_factor = 0.25`
* `imbalance_limit = 0.05`
