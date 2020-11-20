
# System Parameters

A operational description of Checker's internal parameters, and operations on them.

## State

* `q`: of type (1 / kit).
* `index`: of type tez.
* `protected_index`:  of type tez.
* `target`: TODO: said dimensionless, but I think tez/kit? Hmmm. I have to re-check the units of measure.
* `drift'`
* `drift`
* `outstanding_kit`: approximation of the total amount of kit that would be currently required to close all burrows.
* `circulating_kit`: approximation of the total amount of kit that is currently in circulation.
* `last_touched`: the last time the parameters were touched.

And two additional indices, one used in the calculation of _burrowing fees_ and one in the calculation of the _imbalance adjustment_:
* `burrow_fee_index`
* `imbalance_index`

Q1: We discussed the possibility of storing `1/q` instead of `q` in Checker's state. Would that be beneficial in any way?

Q2: How much can `outstanding_kit` and `circulating_kit` deviate from reality? How big an issue can that be?

## Initialization

We currently initialize checker with the following parameters:

```
q = 1
index = 1xtz
protected_index = 1xtz
target = 1
drift = 0
drift' = 0
outstanding_kit = 1kit
circulating_kit = 1kit
last_touched = now
burrow_fee_index = 1
imbalance_index  = 1
```

Q3: What should the starting values for `outstanding_kit` and `circulating_kit` be? For their calculations we only use multiplication with factors (see below), so starting with honest zero values would pin them to zero forever.


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
The definition of `tz_minting` and `tz_liquidation` implies that at any given moment, `tz_minting >= tz_liquidation`. Combined with `fminting > fliquidation`, we have that
```
tz_minting * fminting > tz_liquidation * fliquidation
```
which is useful in liquidation logic (see burrow-state-liquidations.md).

## Adjustment index

The adjustment index, as required by burrowing logic, can be calculated from the system parameters as the product of the burrow fee index and the imbalance index:
```
adjustment_index = burrow_fee_index * imbalance_index
```

## Touching

Touching the system parameters has the effect of updating all aforementioned fields, and calculating the burrowing fees that need to be accrued to the uniswap sub-contract. This is done under the assumption that we have available the current time `now`, the current index `index_now` (calculated by the medianizer), and the current price of kit in tez `kit_in_tez_now` (calculated by the uniswap sub-contract). We update each field:

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
where `low` and `high` depend on how much time has passed since the last time the parameters were touched, effectively limiting how fast `protected_index` can change:
```
low  = exp ( epsilon * (now - last_touched))
high = exp (-epsilon * (now - last_touched))
```
**NOTE**: `exp (x) = 1 + x` here; we expect the contract to be touched rather frequently, which keeps the exponent rather small, which makes this a good approximation of `exp`.

### `drift'`
For the calculation of the derivative of `drift`, `drift'`, we only use the last-observed target (TODO: show how we get from the original formula with the logarithms to this?) We calculate as follows:
```
new_drift' =
  -0.0005 / (secs_in_a_day ^ 2) , if                        target <= exp (-high_bracket)
  -0.0001 / (secs_in_a_day ^ 2) , if exp (-high_bracket) <  target <= exp (-low_bracket)
   0                            , if exp (-low_bracket)  <  target <  exp (low_bracket)
   0.0001 / (secs_in_a_day ^ 2) , if exp ( low_bracket)  <= target <  exp ( high_bracket)
   0.0005 / (secs_in_a_day ^ 2) , if exp ( high_bracket) <= target
```

TODO: Eventually we should pre-calculate all constant things (e.g. `exp (-low_bracket)`).

### `drift`
For the calculation of the current drift, we use use the following formula:
```
new_drift = old_drift + (1/2) * (old_drift' + new_drift') * (now - last_touched)
```

 ### `q`
For the calculation of the current quantity, we use use the following formula:
```
new_q = old_q
      * exp (
          (old_drift + (1/6) * ((2 * old_drift') + new_drift') * (now - last_touched))
          * (now - last_touched)
        )
```

**NOTE**: `exp (x) = 1 + x` here; TODO: not sure if the exponent is small enough for this to be a good approximation.

### `target`
```
new_target = new_q * (new_index / kit_in_tez_now)
```

### `burrow_fee_index`

The burrow fee index is updated linearly on the number of seconds that have passed since the last time the parameters were touched.
```
new_burrow_fee_index = old_burrow_fee_index
                     * (1 + burrow_fee_rate * (now - last_touched) / seconds_in_a_year)
```

### `imbalance_index`

The imbalance index is also updated linearly on the number of seconds that have passed since the last time the parameters were touched
```
new_imbalance_index = old_imbalance_index
                    * (1 + imbalance_rate * (now - last_touched) / seconds_in_a_year)
```
but `imbalance_rate` varies, depending on the difference between `old_outstanding_kit` and `old_circulating_kit`:
```
imbalance_rate =
  min( 5 * outstanding, (outstanding - circulating)) * 0.01 / outstanding , if outstanding >= circulating
  max(-5 * outstanding, (outstanding - circulating)) * 0.01 / outstanding , if outstanding <  circulating
```

###  Intermediate `outstanding_kit`
In order to compute the updates for the two remaining fields (`outstanding_kit` and `circulating_kit`), we first need to calculate the current amount of kit outstanding, taking into account the accrued burrowing fee, thus
```
outstanding_with_fees = old_outstanding_kit * (new_burrow_fee_index / old_burrow_fee_index)
```

### Accrual to uniswap
The accrued burrowing fees are to be given to the uniswap sub-contract. The total amount we easily compute as
```
accrual_to_uniswap = outstanding_with_fees - old_outstanding
```

### `outstanding_kit`
To obtain the updated `outstanding_kit`, we need to account for both the accrued burrowing fees, and the imbalance adjustment
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
Finally, to obtain the up-to-date `circulating_kit`, we just need to record the new kit in circulation, that is, `accrual_to_uniswap`:
```
new_circulating_kit = old_circulating_kit + accrual_to_uniswap
```

**NOTE**: If the current timestamp is identical to that stored in the parameters, we do not perform any of the above.

## Misc

* `seconds_in_a_year = 31556952 (= (365 + 1/4 - 1/100 + 1/400) days * 24 * 60 * 60)`
* `seconds_in_a_day  = 86400 (= 24 * 60 * 60)`
* `low_bracket = 0.005`
* `high_bracket = 0.05`
