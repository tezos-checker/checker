
# Burrow State & Liquidation

George's operational interpretation of the burrow state and operations on it.

## State

* `outstanding`: the amount of kit that is outstanding from the burrow. This **does not** take into account kit we expect to receive (to burn) from pending auctions. However, `outstanding` does increase over time, since the burrowing fee and the adjustment fee are added to it. So, effectively, before doing anything else with a burrow, we update its state ("touch" it, see below).
* `collateral`: the amount of tez stored in the burrow. Collateral that has been sent to auctions **does not** count towards this amount; for all we know, it's gone forever.
* `collateral_at_auction`: the total amount of tez that has been sent to auctions from this burrow, to be sold for kit.

Additional fields:
* `timestamp`: the last time the burrow was touched.
* `adjustment_index`: the last observed adjustment index (at time `timestamp`).
* `has_creation_deposit`: whether the burrow is supported by a creation deposit. If not, it's considered "inactive".

## Touching

First thing to do before considering any of the things below is to update the state of the burrow, by touching it. The effect of this is to
* Update the timestamp in the burrow to reflect the last time it was touched
  ```
  new_timestamp   = now()
  ```
* To add accrued burrow and adjustment fee to its outstanding kit
  ```
  new_outstanding = old_outstanding * (new_adjustment_index / old_adjustment_index)
  ```
**TODO** Take into account the time difference between touches.

So, for the remainder, let's assume that the burrow has been touched, and its current state is up-to-date.

Q1: When the burrow is touched and we must update its outstanding kit, do we compute the burrowing fee and the imbalance fee on `outstanding_kit` or on `outstanding_kit - repaid_kit`?
```
new_outstanding = old_outstanding * (new_adjustment_index / old_adjustment_index)
new_outstanding = (old_outstanding - repaid_kit) * (new_adjustment_index / old_adjustment_index)
```
I assume the first of the two but better ask.

## Is a burrow collateralized (i.e. not overburrowed)

The burrow is considered collateralized if the following holds:
```
collateral >= outstanding * fminting * current_minting_price      (1)
```
`collateral` here refers to the amount of tez stored in the burrow (collateral that has been sent to auctions **does not** count towards this amount; for all we know, it's gone forever).

`outstanding` here refers to the accrued amount of kit that is outstanding from the burrow (kit we expect to receive from pending auctions **is not** considered burned here, but still outstanding).

## Is a burrow not a candidate for liquidation
The burrow cannot be marked for liquidation if the following holds:
```
collateral >= optimistic_outstanding * fliquidation * liquidation_price      (2)
```
`collateral` here refers to the amount of tez stored in the burrow (collateral that has been sent to auctions **does not** count towards this amount; for all we know, it's gone forever).

`outstanding` here refers to the accrued amount of kit that is outstanding from the burrow. In this case we optimistically **do take into account** kit we expect to receive from pending auctions at the `current_minting_price`. That is
```
optimistic_outstanding = outstanding - (collateral_at_auction / current_minting_price)
```

Q2: Is the above calculation correct? It assumes that the liquidation was not warranted (see below), and we will thus receive all the kit that corresponds to the collateral we have sent to auctions. Is that OK or should we compute
```
optimistic_outstanding = outstanding - (90% * collateral_at_auction / current_minting_price)
```
instead, assuming that the liquidation **is** warranted?

## How much collateral should we liquidate

Say the burrow has been touched and all its parameters are up to date. Concerning liquidation, we have the following cases:

### Case 1: The burrow is not a candidate for liquidation
If (2) above holds then the burrow should not be liquidated. Send nothing to auctions and leave the burrow as is.

### Case 2: The burrow is a candidate for liquidation
If (2) above does not hold, then the burrow should be liquidated. Either partially, completely, or even be closed.

First things first, the actor who initiated liquidation should get their reward (burrow creation deposit + percentage of collateral):
```
liquidation_reward = creation_deposit + (collateral * liquidation_reward_percentage)
```
That is, before we compute anything else, we leave the burrow with less collateral and without a creation deposit:
```
has_creation_deposit = false
collateral           = collateral - (collateral * liquidation_reward_percentage)
```
Now, depending on how much collateral remains, we have the following cases:

#### Case 2A: `collateral < creation_deposit`
We cannot replenish the creation deposit.
* We send all the remaining collateral to be auctioned off for kit
* We reset and "deactivate" the burrow:
  ```
  has_creation_deposit  = false
  collateral            = 0
  outstanding           = 0
  collateral_at_auction = 0
  ```

**NOTE**: I assume that the system shall burn the kit it receives from selling the tez right away.

#### Case 2B: `collateral >= creation_deposit`
We can replenish the creation deposit, and this is the first thing we do:
```
collateral = collateral - creation_deposit
```

Now all that remains is to compute what should we auction off to bring the burrow to a state where _"any outstanding kits could have just been minted"_, assuming that all tez in auctions will optimistically be sold for `current_minting_price`:
```
optimistic_outstanding = outstanding - (collateral_at_auction / current_minting_price)
```
Using (1) from earlier, we basically require the following to hold between `tez_to_auction` and `repaid_kit`:
```
tez_to_auction = repaid_kit * current_minting_price
collateral - tez_to_auction = (optimistic_outstanding - expected_kit) * fminting * current_minting_price
```
which, once solved, gives:
```
tez_to_auction = (optimistic_outstanding * fminting * current_minting_price - collateral) / (fminting - 1)
repaid_kit     = tez_to_auction / current_minting_price
```

* If `optimistic_outstanding * fminting * current_minting_price < collateral` then restoration is impossible: liquidate the entire remaining collateral:
  ```
  has_creation_deposit  = true
  collateral            = 0
  collateral_at_auction = collateral_at_auction + collateral
  ```
* If `optimistic_outstanding * fminting * current_minting_price >= collateral` then add a penalty of 10% to `tez_to_auction`:
  ```
  tez_to_auction = tez_to_auction * (1 + punishment)
  ```
  If this means that `tez_to_auction > collateral` then, again, liquidate the entire remaining collateral:
  ```
  has_creation_deposit  = true
  collateral            = 0
  collateral_at_auction = collateral_at_auction + collateral
  ```
* Otherwise auction off exactly `tez_to_auction`:
  ```
  has_creation_deposit  = true
  collateral            = collateral - tez_to_auction
  collateral_at_auction = collateral_at_auction + tez_to_auction
  ```

## Was the liquidation warranted

We sent 10% extra tez to be auctioned off as a penalty, but in case the actual selling price of the tez would not have triggered a liquidation (retrospectively), we wish to bring that back to the burrow, if possible.

Calculations:
In order to see whether liquidation should occur, we used equation (2) above, which we can rewrite as
```
liquidation_price <= collateral / (optimistic_outstanding * fliquidation)    (3)
```
So, if (3) was satisfied, we wouldn't have triggered a liquidation. If we assume that at the end we sent `tez_to_auction` to be auctioned off and we received `repaid_kit` for it, we have:
```
maximum_non_liquidating_price = collateral / (optimistic_outstanding * fliquidation)
real_price                    = tez_to_auction / repaid_kit    # derived from the auction outcome
```

If `real_price <= maximum_non_liquidating_price` then the liquidation was not warranted (i.e. the liquidation price we used when calculating `tez_to_auction` was off) and we wish to return the kit we received from the auction in its entirety to the burrow:
```
real_price <= maximum_non_liquidating_price
tez_to_auction / repaid_kit <= collateral / (fliquidation * optimistic_outstanding) <=>
tez_to_auction * (fliquidation * optimistic_outstanding) <= repaid_kit * collateral <=>
tez_to_auction * (fliquidation * optimistic_outstanding) / collateral <= repaid_kit <=>
repaid_kit >= tez_to_auction * (fliquidation * optimistic_outstanding) / collateral
```
So, if the kit that the auction yields is more than
```
min_received_kit_for_unwarranted = tez_to_auction * (fliquidation * optimistic_outstanding) / collateral
```
then this liquidation was unwarranted.

## What if the liquidation was warranted

When we send `tez_to_auction` to an auction, we also send `min_received_kit_for_unwarranted` so that---after the auction is over---we can determine whether it was warranted. If it was warranted, then we wish to return the received kit in its entirety to the burrow. Otherwise we burn 10% of the kit earnings.

The auction logic might end up splitting `tez_to_auction` into parts (slices) that can be sold for different prices; we perform the above check per slice.
```
tez_to_auction = tez_1 + tez_2 + ... + tez_n
```
If we end up selling slice `tez_i` for `kit_i`, this part of the liquidation is considered unwarranted (and thus `kit_i` is returned to the burrow) only if
```
kit_i >= min_received_kit_for_unwarranted * (tez_i / tez_to_auction) <=>
tez_to_auction * kit_i >= min_received_kit_for_unwarranted * tez_i
```

## Misc

* I have replaced `fplus` with `fminting` and `fminus` with `fliquidation`; I think they are more descriptive.
* `fminting > fliquidation`
* `minting_price >= liquidation_price`
* `punishment = 10%`

