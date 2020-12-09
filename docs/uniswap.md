
# Uniswap sub-contract

An operational interpretation of the uniswap API inside the checker contract, and operations on it.

## State

* `tez`: the total amount of tez currently held by the uniswap contract (in mutez).
* `kit`: the total amount of kit currently held by the uniswap contract (in mukit).
* `lqt`: the total number of liquidity tokens held by the uniswap contract (as an int).

Additional fields:
* `kit_in_tez_in_prev_block`: the price of kit in tez (`kit / tez`) at the end of the previous block block (as a rational).
* `last_level`: the last block that the uniswap contract was touched on.

**NOTE 1**: The reason we store `kit_in_tez_in_prev_block` and `last_level` in the state of uniswap is security. When the price implied by uniswap is queried to compute the drift derivative (see system-parameters.md), we don't want to give the current price, but instead return the last price at the end of the previous block. This makes it just a little harder to manipulate these small price fluctuations.

**NOTE 2**: `kit_in_tez_in_prev_block` is always computed as the amount of kit divided by the amount of tez, so it can never really grow too much in size. Hence we use a lossless rational for its representation.

## Initialization

When the system starts, all parameters are set to zero. Given that Checker get on the chain at level `lvl`, we initialize the parameters thus:
```
tez                      = 0
kit                      = 0
lqt                      = 0
kit_in_tez_in_prev_block = 0/0     # same as kit/tez now: Undefined
last_level               = lvl
```
Effectively this means that initially we cannot use uniswap for more-or-less anything (including querying the price of kit in tez). The only way to change this is by adding liquidity to it.

## General notes on the interfaces

* None of the interfaces below refers to prices. Instead, we pass inputs, and minimum and maximum expected values for things (e.g. kit, tez, liquidity tokens, or time). If the criteria cannot be met the operations fail. This agrees with e.g. the API offered by Dexter.

* All the following happen within the smart contract, which means that in the calculations below we often refer to `amount` (the amount of tez given), `level` (the current block height), as well as `now` (the timestamp of the current block, as provided by this block's baker).

## Adding liquidity

First things first: if `last_level < now`, it means that this is the first time that the uniswap contract is touched in this block, so we update `kit_in_tez_in_prev_block` to the price observed now, and set `last_level` to the current height, so that we don't update `kit_in_tez_in_prev_block` again in this block:
```
kit_in_tez_in_prev_block = tez/ kit
last_level               = level
```
If `last_level = now`, then we don't perform the update; this is not the first time we've touched the uniswap contract in this block.

QQ: Is it possible that `last_level > now`? If yes, what does it mean, and how do we handle it?

**Inputs**
* `max_kit_deposited`: The maximum amount of kit to be added to the uniswap contract
* `min_lqt_minted`: The minimum number of liquidity tokens expected to be received.
* `deadline`: The deadline; starting from this timestamp the transaction cannot be executed.

If any of the following holds, the transaction fails
* If we are on or past the deadline (`now >= dealine`), the transaction fails.
* If no tez is given (`amount = 0`), then no liquidity will be provided (see calculations below), so the transaction fails.
* If no kit is offered (`max_kit_deposited = 0`), the transaction fails.
* If no liquidity is to be added (`min_lqt_minted = 0`), the transaction fails.

If `lqt = 0` is zero---which means that there is no liquidity in the contract---then this provider is the first "First Liquidity Provider". Otherwise, it is the "Non-first Liquidity Provider". After we perform all the above checks thus, we operate differently based on whether we have a first or a non-first liquidity provider.

QQ: When `lqt = 0`, it means that the uniswap contract is deprived of all its liquidity tokens. The obvious case is when it is in its initial state, but I think it can also get there if all Liquidity Providers remove all liquidity. How should we deal with that case?

### First Liquidity Provider

For the first liquidity provider, we require (`amount >= 1`), to agree with Dexter's interface. If that's not the case, the transaction fails.

We calculate the liquidity to be created as the amount of tez given, floored:
```
lqt_minted = floor(amount)
```
If `lqt_minted < min_lqt_minted`, then the transaction fails. Otherwise, we proceed with updating the parameters:
```
kit = max_kit_deposited
tez = amount
lqt = lqt_minted
```
For the contract to become immediately useful, we also set the remaining two fields as well, using **the current values**:
```
kit_in_tez_in_prev_block = amount / max_kit_deposited
last_level = level
```
This means that `kit_in_tez_in_prev_block` does not refer to the "price of kit in tez at the end of the last block"; that would be `0/0`. That is exactly the reason we do this here: when we query the price of kit in tez we don't want to get `0/0`. Starting from the next block though, `kit_in_tez_in_prev_block` remains "one block behind", compared to the current `tez/kit` ratio, as expected.

Note that the complete `amount` and `max_kit_deposited` are consumed; there are no leftovers:
```
tez_to_return = 0
kit_to_return = 0
```

### Non-first Liquidity Provider

For the non-first liquidity provider, we calculate the number of liquidity tokens to mint and the amount of kit that needs to be deposited using the ratio of the provided tez vs. the tez currently in the uniswap contract:
```
lqt_minted    = lqt * (amount / tez)    # floor, as an integer
kit_deposited = kit * (amount / tez)    # ceil, in mukit
```
Because of this calculation, if the pool of tez is empty (`tez = 0`), the transaction fails. Also
* If `lqt_minted < min_lqt_minted` then the transaction fails.
* If `max_kit_deposited < kit_deposited` then the transaction fails.
* If `kit_deposited = Kit.zero` then the transaction fails.

If all is good, we proceed with updating the parameters
```
kit = kit + kit_deposited
tez = tez + amount
lqt = lqt + lqt_minted
```
Note that the complete `amount` is consumed. However,  `kit_deposited` might differ from `max_kit_deposited`. Hence, we return the leftovers:
```
tez_to_return = 0
kit_to_return = max_kit_deposited - kit_deposited
```

## Removing liquidity

First things first: if `last_level < now`, it means that this is the first time that the uniswap contract is touched in this block, so we update `kit_in_tez_in_prev_block` to the price observed now, and set `last_level` to the current height, so that we don't update `kit_in_tez_in_prev_block` again in this block:
```
kit_in_tez_in_prev_block = tez/ kit
last_level               = level
```
If `last_level = now`, then we don't perform the update; this is not the first time we've touched the uniswap contract in this block.

QQ: Is it possible that `last_level > now`? If yes, what does it mean, and how do we handle it?

**Inputs**
* `lqt_burned`: The number of liquidity tokens to be removed from the uniswap contract.
* `min_tez_withdrawn`: The minimum amount of tez to be received for the removed liquidity tokens.
* `min_kit_withdrawn`: The minimum amount of kit to be received for the removed liquidity tokens.
* `deadline`: The deadline; starting from this timestamp the transaction cannot be executed.

If any of the following holds, the transaction fails
* If there are no liquidity tokens available (`lqt = 0`), the transaction fails.
* If the amount of tez given is non-zero (`amount <> 0`), the transaction fails.
* If we are on or past the deadline (`now >= deadline`), the transaction fails.
* If no liquidity tokens are to be removed (`lqt_burned = 0`), the transaction fails.
* If no tez is expected to be received from this transaction (`min_tez_withdrawn = 0`), the transaction fails.
* If no kit is expected to be received from this transaction (`min_kit_withdrawn = 0`), the transaction fails.

Otherwise, we compute how much tez and kit should be returned, using the ratio of the provided liquidity tokens vs. the number of liquidity tokens currently in the uniswap contract:
```
tez_withdrawn = tez * (lqt_burned / lqt)    # floor, in mutez
kit_withdrawn = kit * (lqt_burned / lqt)    # floor, in mukit
```
Also, we check that the bounds are respected:
* If `tez_withdrawn < min_tez_withdrawn`, the transaction fails.
* If `tez_withdrawn > tez`, the transaction fails.
* If `kit_withdrawn < min_kit_withdrawn`, the transaction fails.
* If `kit_withdrawn > kit`, the transaction fails.
* If `lqt_burned > lqt`, the transaction fails.

If all is good, we proceed with updating the parameters
```
kit = kit - kit_withdrawn
tez = tez - tez_withdrawn
lqt = lqt - lqt_burned
```
and return the withdrawn amounts:
```
tez_to_return = tez_withdrawn
kit_to_return = kit_withdrawn
```

QQ: Do we allow the removal of liquidity, even if it means that the uniswap contract will remain with zero liquidity? The checks above ensure that we don't get below zero, but allow zero just fine.

## Buying Kit

First things first: if `last_level < now`, it means that this is the first time that the uniswap contract is touched in this block, so we update `kit_in_tez_in_prev_block` to the price observed now, and set `last_level` to the current height, so that we don't update `kit_in_tez_in_prev_block` again in this block:
```
kit_in_tez_in_prev_block = tez/ kit
last_level               = level
```
If `last_level = now`, then we don't perform the update; this is not the first time we've touched the uniswap contract in this block.

QQ: Is it possible that `last_level > now`? If yes, what does it mean, and how do we handle it?

**Inputs**
* `min_kit_expected`: The minimum amount of kit to be bought.
* `deadline`: The deadline; starting from this timestamp the transaction cannot be executed.

If any of the following holds, the transaction fails
* If the pool of tez is empty (`tez = 0`), the transaction fails.
* If the pool of kit is empty (`kit = 0`), the transaction fails.
* If the amount of tez given is zero (`amount = 0`), the transaction fails.
* If we are on or past the deadline (`now >= dealine`), the transaction fails.
* If no amount of kit is expected (`min_kit_expected = 0`), the transaction fails.

Otherwise, we compute how much kit can be bought for the `amount` of tez as follows:
```
price      = kit / tez
slippage   = tez / (tez + amount)
kit_bought = amount * price * slippage * (1 - uniswap_fee)   # floor, in mukit
```
Also, we check that the bounds are respected:
* If `kit_bought < min_kit_expected`, the transaction fails.
* If `kit_bought> kit`, the transaction fails.

If all is good, we proceed with updating the parameters
```
kit = kit - kit_bought
tez = tez + amount
```
and return the bought amount of kit:
```
kit_to_return = kit_bought
```

QQ: Are we OK with this operation leaving the contract without any kit? Is that even possible or is that prevented in a way I am missing?

## Selling Kit

First things first: if `last_level < now`, it means that this is the first time that the uniswap contract is touched in this block, so we update `kit_in_tez_in_prev_block` to the price observed now, and set `last_level` to the current height, so that we don't update `kit_in_tez_in_prev_block` again in this block:
```
kit_in_tez_in_prev_block = tez/ kit
last_level               = level
```
If `last_level = now`, then we don't perform the update; this is not the first time we've touched the uniswap contract in this block.

QQ: Is it possible that `last_level > now`? If yes, what does it mean, and how do we handle it?

**Inputs**
* `kit_given`: The amount of kit to be sold to the uniswap contract.
* `min_tez_expected`: The minimum amount of tez to be bought.
* `deadline`: The deadline; starting from this timestamp the transaction cannot be executed.

If any of the following holds, the transaction fails
* If the pool of tez is empty (`tez = 0`), the transaction fails.
* If the pool of kit is empty (`kit = 0`), the transaction fails.
* If the amount of kit given is zero (`kit_given = 0`), the transaction fails.
* If we are on or past the deadline (`now >= dealine`), the transaction fails.
* If no amount of tez is expected (`min_tez_expected = 0`), the transaction fails.

Otherwise, we compute how much tez can be bought for the `kit_given` as follows:
```
price      = tez / kit
slippage   = kit / (kit + kit_given)
tez_bought = kit * price * slippage * (1 - uniswap_fee)   # floor, in mutez
```
Also, we check that the bounds are respected:
* If `tez_bought < min_tez_expected`, the transaction fails.
* If `tez_bought> tez`, the transaction fails.

If all is good, we proceed with updating the parameters
```
kit = kit + kit_given
tez = tez - tez_bought
```
and return the bought amount of tez:
```
tez_to_return = tez_bought
```
**NOTE**: There are more than one ways to calculate things when buying and selling kit. Given that `da` amount of one quantity is given, what we do essentially computes first what should the return be for the product of quantities kept by uniswap to stay the same:
```
db = da * (b / (a + da))
```
and then keeps `fee` of that, thus returning `db` calculated instead like this:
```
db = da * (b / (a + da)) * (1 - fee)
```
Dexter takes an alternative approach, where the fee is (conceptually, at least) on the amount given. That is, the returned amount is
```
db = da' * (b / (a + da'))
```
where
```
da' = da * (1 - fee)
```
The two calculations give slightly different results, but hopefully that is not a problem.

QQ: Are we OK with this operation leaving the contract without any tez? Is that even possible or is that prevented in a way I am missing?

## Misc

* `uniswap_fee = 0.002`

QQ: In Dexter the uniswap fee is 0.003 instead of 0.002. Which one shall we choose?

