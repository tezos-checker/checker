CFMM subsystem
==============

**NOTE**: CFMM stands for *Constant Function Market Maker*. What this
means is that when parties exchange kit for ctez and vice versa, using
checker, checker tries to keep the product of kit and ctez within it
unchanged (ignoring the fees of course).

This file gives an operational interpretation of the cfmm API inside the
checker contract, and operations on it.

State
-----

-  ``ctez``: the total amount of ctez currently held by the cfmm contract.
-  ``kit``: the total amount of kit currently held by the cfmm contract.
-  ``lqt``: the total amount of liquidity held by the cfmm contract.

Additional fields:

* ``kit_in_ctez_in_prev_block``: the price of kit in ctez (``kit / ctez``)
  at the end of the previous block (as a ratio).

*  ``last_level``: the last block that the cfmm contract was touched on (as
   a nat).

**NOTE 1**: The reason we store ``kit_in_ctez_in_prev_block`` and
``last_level`` in the state of cfmm is security. When the price implied
by cfmm is queried to compute the drift derivative (see
system-parameters.md), we don’t want to give the current price, but
instead return the last price at the end of the previous block. This
makes it just a little harder to manipulate these small price
fluctuations.

**NOTE 2**: ``kit_in_ctez_in_prev_block`` is always computed as the
amount of kit divided by the amount of ctez, so it can never really grow
too much in size. Hence we use a lossless rational for its
representation.

Initialization
--------------

When the system starts, all parameters are set to the lowest non-zero amount.
Given that Checker gets deployed on the chain at level ``lvl``, we initialize
the parameters thus:

::

   ctez                      = 1
   kit                       = 1
   lqt                       = 1
   kit_in_ctez_in_prev_block = 1     # same as kit/ctez now
   last_level                = lvl

Effectively, given that (a) no one can remove the first liquidity token
and (b) how rounding works in the operations that follow, the contract
will never be completely out of ctez, kit, or liquidity. So,
setting the initial values to one removes the need for division-by-zero
checks, and the first/non-first liquidity provider distinction that is
e.g. adopted by Dexter. Of course, this price is only for the beginning,
and it is expected that through trading it will eventually move closer
to the *real* price.

General notes on the interfaces
-------------------------------

-  None of the interfaces below refers to prices. Instead, we pass
   inputs, and minimum and maximum expected values for things (e.g. kit,
   ctez, liquidity, or time). If the criteria cannot be met the
   operations fail. This agrees with e.g. the API offered by Dexter.

-  All the following happen within the smart contract, which means that
   in the calculations below we often refer to ``level`` (the current
   block height), as well as ``now`` (the timestamp of the current
   block, as provided by this block’s baker).

Adding liquidity
----------------

First things first: if ``last_level < level``, it means that this is the
first time that the cfmm contract is touched in this block, so we update
``kit_in_ctez_in_prev_block`` to the price observed now, and set
``last_level`` to the current height, so that we don’t update
``kit_in_ctez_in_prev_block`` again in this block:

::

   kit_in_ctez_in_prev_block = ctez/kit
   last_level                = level

If ``last_level = level``, then we don’t perform the update; this is not
the first time we’ve touched the cfmm contract in this block.

Inputs
~~~~~~

- ``ctez_amount``: The amount of ctez to be added to the cfmm contract.

- ``max_kit_deposited``: The maximum amount of kit to be added to the cfmm
  contract.

- ``min_lqt_minted``: The minimum amount of liquidity expected to be
  received.

- ``deadline``: The deadline; starting from this timestamp the transaction can
  no longer be executed.

If any of the following holds, the transaction fails:

- If we are on or past the deadline (``now >= deadline``), the transaction
  fails.

- If no ctez is given (``ctez_amount = 0``), the transaction fails.

- If no kit is offered (``max_kit_deposited = 0``), the transaction fails.

- If no liquidity is to be added (``min_lqt_minted = 0``), the transaction
  fails.

So, we calculate the amount of liquidity to mint and the amount of kit that
needs to be deposited using the ratio of the provided ctez vs. the ctez
currently in the cfmm contract:

::

   lqt_minted    = lqt * (ctez_amount / ctez)    # floor
   kit_deposited = kit * (ctez_amount / ctez)    # ceil

Because of this calculation, we need to know that the pool of ctez is
not empty, but this should be ensured by the initial setup of the cfmm
sub-contract. Also

- If ``lqt_minted < min_lqt_minted`` then the transaction fails.
- If ``max_kit_deposited < kit_deposited`` then the transaction fails.
- If ``kit_deposited = Kit.zero`` then the transaction fails.

If all is good, we proceed with updating the parameters

::

   kit  = kit + kit_deposited
   ctez = ctez + ctez_amount
   lqt  = lqt + lqt_minted

Note that the complete ``ctez_amount`` is consumed. However,
``kit_deposited`` might differ from ``max_kit_deposited``. Hence, we
return the leftovers:

::

   kit_to_return  = max_kit_deposited - kit_deposited

Removing liquidity
------------------

First things first: if ``last_level < level``, it means that this is the
first time that the cfmm contract is touched in this block, so we update
``kit_in_ctez_in_prev_block`` to the price observed now, and set
``last_level`` to the current height, so that we don’t update
``kit_in_ctez_in_prev_block`` again in this block:

::

   kit_in_ctez_in_prev_block = ctez/kit
   last_level                = level

If ``last_level = level``, then we don’t perform the update; this is not
the first time we’ve touched the cfmm contract in this block.

Inputs
~~~~~~

- ``lqt_burned``: The amount of liquidity to be removed from the cfmm contract.
- ``min_ctez_withdrawn``: The minimum amount of ctez to be received for the removed liquidity.
- ``min_kit_withdrawn``: The minimum amount of kit to be received for the removed liquidity.
- ``deadline``: The deadline; starting from this timestamp the transaction can no longer be executed.

If any of the following holds, the transaction fails

- If we are on or past the deadline (``now >= deadline``), the transaction fails.
- If no liquidity is to be removed (``lqt_burned = 0``), the transaction fails.
- If no ctez is expected to be received from this transaction (``min_ctez_withdrawn = 0``), the transaction fails.
- If no kit is expected to be received from this transaction (``min_kit_withdrawn = 0``), the transaction fails.

Otherwise, we compute how much ctez and kit should be returned, using the ratio
of the provided liquidity vs. the liquidity currently in the cfmm contract:

::

   ctez_withdrawn = ctez * (lqt_burned / lqt)   # floor
   kit_withdrawn  = kit  * (lqt_burned / lqt)   # floor

Also, we check that the bounds are respected:

* If ``ctez_withdrawn < min_ctez_withdrawn``, the transaction fails.
* If ``ctez_withdrawn > ctez``, the transaction fails.
* If ``kit_withdrawn < min_kit_withdrawn``, the transaction fails.
* If ``kit_withdrawn > kit``, the transaction fails.
* If ``lqt_burned > lqt``, the transaction fails.

If all is good, we proceed with updating the parameters

::

   kit  = kit  - kit_withdrawn
   ctez = ctez - ctez_withdrawn
   lqt  = lqt  - lqt_burned

and return the withdrawn amounts:

::

   ctez_to_return = ctez_withdrawn
   kit_to_return  = kit_withdrawn

Buying Kit
----------

First things first: if ``last_level < level``, it means that this is the
first time that the cfmm contract is touched in this block, so we update
``kit_in_ctez_in_prev_block`` to the price observed now, and set
``last_level`` to the current height, so that we don’t update
``kit_in_ctez_in_prev_block`` again in this block:

::

   kit_in_ctez_in_prev_block = ctez/kit
   last_level                = level

If ``last_level = level``, then we don’t perform the update; this is not
the first time we’ve touched the cfmm contract in this block.

Inputs
~~~~~~

- ``ctez_amount``: The amount of ctez to be added to the cfmm contract.
- ``min_kit_expected``: The minimum amount of kit to be bought.
- ``deadline``: The deadline; starting from this timestamp the transaction can no longer be executed.

If any of the following holds, the transaction fails

- If the amount of ctez given is zero (``ctez_amount = 0``), the transaction fails.
- If we are on or past the deadline (``now >= deadline``), the transaction fails.
- If no amount of kit is expected (``min_kit_expected = 0``), the transaction fails.

Otherwise, we compute how much kit can be bought for the ``ctez_amount``
of ctez as follows:

::

   price      = kit / ctez
   slippage   = ctez / (ctez + ctez_amount)
   kit_bought = ctez_amount * price * slippage * (1 - cfmm_fee)   # floor

Also, we check that the bounds are respected:

* If ``kit_bought < min_kit_expected``, the transaction fails.
* If ``kit_bought > kit``, the transaction fails.

If all is good, we proceed with updating the parameters

::

   kit  = kit  - kit_bought
   ctez = ctez + ctez_amount

and return the bought amount of kit:

::

   kit_to_return = kit_bought

Selling Kit
-----------

First things first: if ``last_level < level``, it means that this is the
first time that the cfmm contract is touched in this block, so we update
``kit_in_ctez_in_prev_block`` to the price observed now, and set
``last_level`` to the current height, so that we don’t update
``kit_in_ctez_in_prev_block`` again in this block:

::

   kit_in_ctez_in_prev_block = ctez/kit
   last_level                = level

If ``last_level = level``, then we don’t perform the update; this is not
the first time we’ve touched the cfmm contract in this block.

Inputs
~~~~~~

- ``kit_given``: The amount of kit to be sold to the cfmm contract.
- ``min_ctez_expected``: The minimum amount of ctez to be bought.
- ``deadline``: The deadline; starting from this timestamp the transaction can no longer be executed.

If any of the following holds, the transaction fails

- If the amount of kit given is zero (``kit_given = 0``), the transaction fails.
- If we are on or past the deadline (``now >= deadline``), the transaction fails.
- If no amount of ctez is expected (``min_ctez_expected = 0``), the transaction fails.

Otherwise, we compute how much ctez can be bought for the ``kit_given``
as follows:

::

   price       = ctez / kit
   slippage    = kit / (kit + kit_given)
   ctez_bought = kit * price * slippage * (1 - cfmm_fee)   # floor

Also, we check that the bounds are respected:

* If ``ctez_bought < min_ctez_expected``, the transaction fails.
* If ``ctez_bought > ctez``, the transaction fails.

If all is good, we proceed with updating the parameters

::

   kit  = kit  + kit_given
   ctez = ctez - ctez_bought

and return the bought amount of ctez:

::

   ctez_to_return = ctez_bought

**NOTE**: There are more than one ways to calculate things when buying
and selling kit. Given that ``da`` amount of one quantity is given, what
we do essentially computes first what should the return be for the
product of quantities kept by cfmm to stay the same:

::

   db = da * (b / (a + da))

and then keeps ``fee`` of that, thus returning ``db`` calculated instead
like this:

::

   db = da * (b / (a + da)) * (1 - fee)

Dexter takes an alternative approach, where the fee is (conceptually, at
least) on the amount given. That is, the returned amount is

::

   db = da' * (b / (a + da'))

where

::

   da' = da * (1 - fee)

The two calculations give slightly different results, but hopefully that
is not a problem.

Misc
----

-  ``cfmm_fee = 0.002``
