Burrow State & Liquidation
==========================

An operational interpretation of the burrow state and operations on it.

State
-----

- ``active``: whether the burrow is supported by a creation deposit. If not,
  it’s considered “inactive”.
- ``address``: the address of the contract holding the burrow's collateral and
  creation deposit.
- ``delegate``: the delegate for the amount of tez (collateral + creation
  deposit) the burrow holds.
-  ``collateral``: the amount of tez stored in the burrow. Collateral
   that has been sent to auctions **does not** count towards this
   amount; for all we know, it’s gone forever.
-  ``outstanding_kit``: the amount of kit that is outstanding from the
   burrow. This **does not** take into account kit we expect to receive
   (to burn) from pending auctions. However, ``outstanding_kit`` does
   increase over time, since the burrowing fee and the adjustment fee
   are added to it. So, effectively, before doing anything else with a
   burrow, we update its state (“touch” it, see below).
-  ``collateral_at_auction``: the total amount of tez that has been sent
   to auctions from this burrow, to be sold for kit.
- ``last_touched``: the last time the burrow was touched.
- ``adjustment_index``: the last observed adjustment index (at time
  ``last_touched``).

Touching
--------

First thing to do before considering any of the things below is to
update the state of the burrow, by touching it. The effect of this is to

- Update the timestamp in the burrow to reflect the last time it was
  touched

::

   new_last_touched = now

- To add accrued burrow and adjustment fee to its outstanding kit

::

   new_outstanding = old_outstanding * (new_adjustment_index / old_adjustment_index)``

Note that if the current timestamp is identical to that stored in the burrow,
we do not perform any of the above.

Each of the following operations implicitly touch the burrow (i.e., perform the
above updates) before doing anything else.

Is a burrow collateralized (i.e. not overburrowed)?
---------------------------------------------------

The burrow is considered collateralized if the following holds:

::

   collateral >= outstanding * fminting * current_minting_price      (1)

``collateral`` here refers to the amount of tez stored in the burrow
(collateral that has been sent to auctions **does not** count towards
this amount; for all we know, it’s gone forever).

``outstanding_kit`` here refers to the accrued amount of kit that is
outstanding from the burrow (kit we expect to receive from pending
auctions **is not** considered burned here, but still outstanding).

Is a burrow a candidate for liquidation?
----------------------------------------

The burrow cannot be marked for liquidation if the following holds:

::

   collateral >= optimistic_outstanding * fliquidation * liquidation_price      (2)

``collateral`` here refers to the amount of tez stored in the burrow
(collateral that has been sent to auctions **does not** count towards
this amount; for all we know, it’s gone forever).

``outstanding_kit`` here refers to the accrued amount of kit that is
outstanding from the burrow. In this case we optimistically **do take
into account** kit we expect to receive from pending auctions at the
``current_minting_price``, but pessimistically assume that these pending
auctions are warranted (so we lose the liquidation penalty). That is

::

   optimistic_outstanding = outstanding - (1 - liquidation_penalty) * (collateral_at_auction / current_minting_price)

How much collateral should we liquidate?
----------------------------------------

In general, in order to calculate how much should we auction off we
assume that a) this auction is warranted, b) all pending auctions are
also warranted, c) the price we’ll get for everything is
``current_minting_price``. a) and b) effectively mean that we can only
expect ``(1 - liquidation_penalty)`` returns from all auctions
considered. Formally:

-  First, from auctioning we expect to get the current
   ``minting_price``, so, if we send ``tez_to_auction`` and
   ``repaid_kit`` is received, we have

   ::

      tez_to_auction = repaid_kit * minting_price                   <=>
      repaid_kit = tez_to_auction / minting_price                   (3)

-  Second, we assume that the auction is warranted (so we lose the
   liquidation penalty), thus

   ::

      actual_repaid_kit = repaid_kit * (1 - liquidation_penalty)    (4)

-  Third, we consider all pending auctions to be successful, using the
   current ``minting_price``, but also warranted:

   ::

      optimistic_outstanding = outstanding_kit - (1 - liquidation_penalty) * (collateral_at_auction / minting_price)       (5)

-  Fourth, under the above conditions we aim to bring the burrow into a
   state where it’s not oveburrowed anymore, thus

   ::

      collateral - tez_to_auction >= (optimistic_outstanding - actual_repaid_kit) * fminting * minting_price    (6)

Solving ``(3)``, ``(4)``, ``(5)``, and ``(6)`` the above gives us
``(7)``:

::

   tez_to_auction >=
     ( outstanding_kit * fminting * minting_price
     - (1 - liquidation_penalty) * fminting * collateral_at_auction
     - collateral
     )
     /
     ((1 - liquidation_penalty) * fminting - 1)

if ``((1 - liquidation_penalty) * fminting - 1) > 0``.

Say the burrow has been touched and all its parameters are up to date.
Concerning liquidation, we have the following cases:

Case 1: The burrow is not a candidate for liquidation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If (2) above holds then the burrow should not be liquidated. Send
nothing to auctions and leave the burrow as is.

Case 2: The burrow is a candidate for liquidation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If (2) above does not hold, then the burrow should be liquidated. Either
partially, completely, or even be closed.

First things first, the actor who initiated liquidation should get their
reward (burrow creation deposit + percentage of collateral):

::

   liquidation_reward = creation_deposit + (collateral * liquidation_reward_percentage)

That is, before we compute anything else, we leave the burrow with less
collateral and without a creation deposit:

::

   active     = false
   collateral = collateral - (collateral * liquidation_reward_percentage)

Now, depending on how much collateral remains, we have the following
cases:

Case 2A: ``collateral < creation_deposit``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We cannot replenish the creation deposit.

-  We send all the remaining collateral to be auctioned off for kit.
-  The burrow remains deactivated.

::

   collateral            = 0
   collateral_at_auction = collateral_at_auction + tez_to_auction

Case 2B: ``collateral >= creation_deposit``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can replenish the creation deposit, and this is the first thing we
do:

::

   collateral = collateral - creation_deposit

Now all that remains is to compute what should we auction off to bring
the burrow to a state where *“any outstanding kits could have just been
minted”*. For that, we use the ``(7)``:

::

   tez_to_auction = ceil (
     ( outstanding_kit * fminting * minting_price
     - (1 - liquidation_penalty) * fminting * collateral_at_auction
     - collateral
     )
     /
     ((1 - liquidation_penalty) * fminting - 1)
   )

-  If ``tez_to_auction < 0`` or ``tez_to_auction > collateral``, then
   restoration is impossible: liquidate the entire remaining collateral
   (Note that the resulting burrow can be targeted for liquidation one
   last time (with the creation deposit being the only reward).
   Alternatively, we could (rather harshly) liquidate the deposit too
   and close the burrow.):

   ::

      active                = true
      collateral            = 0
      collateral_at_auction = collateral_at_auction + collateral

-  Otherwise auction off exactly ``tez_to_auction``:

   ::

      active                = true
      collateral            = collateral - tez_to_auction
      collateral_at_auction = collateral_at_auction + tez_to_auction

Was the liquidation warranted?
------------------------------

We sent 10% extra tez to be auctioned off as a penalty, but in case the
actual selling price of the tez would not have triggered a liquidation
(retrospectively), we wish to bring that back to the burrow, if
possible.

Calculations: In order to see whether liquidation should occur, we used
equation (2) above, which we can rewrite as

::

   liquidation_price <= collateral / (optimistic_outstanding * fliquidation)    (3)

So, if (3) was satisfied, we wouldn’t have triggered a liquidation. If
we assume that at the end we sent ``tez_to_auction`` to be auctioned off
and we received ``repaid_kit`` for it, we have:

::

   maximum_non_liquidating_price = collateral / (optimistic_outstanding * fliquidation)
   real_price                    = tez_to_auction / repaid_kit    # derived from the auction outcome

If ``real_price <= maximum_non_liquidating_price`` then the liquidation
was not warranted (i.e. the liquidation price we used when calculating
``tez_to_auction`` was off) and we wish to return the kit we received
from the auction in its entirety to the burrow:

::

   real_price <= maximum_non_liquidating_price
   tez_to_auction / repaid_kit <= collateral / (fliquidation * optimistic_outstanding) <=>
   tez_to_auction * (fliquidation * optimistic_outstanding) <= repaid_kit * collateral <=>
   tez_to_auction * (fliquidation * optimistic_outstanding) / collateral <= repaid_kit <=>
   repaid_kit >= tez_to_auction * (fliquidation * optimistic_outstanding) / collateral

So, if the kit that the auction yields is more than

::

   min_received_kit_for_unwarranted = tez_to_auction * (fliquidation * optimistic_outstanding) / collateral

then this liquidation was unwarranted.

What if the liquidation was warranted?
--------------------------------------

When we send ``tez_to_auction`` to an auction, we also send
``min_received_kit_for_unwarranted`` so that—after the auction is
over—we can determine whether it was warranted. If it was warranted,
then we wish to return the received kit in its entirety to the burrow.
Otherwise we burn 10% of the kit earnings.

The auction logic might end up splitting ``tez_to_auction`` into parts
(slices) that can be sold for different prices; we perform the above
check per slice.

::

   tez_to_auction = tez_1 + tez_2 + ... + tez_n

If we end up selling slice ``tez_i`` for ``kit_i``, this part of the
liquidation is considered unwarranted (and thus ``kit_i`` is returned to
the burrow) only if

::

   kit_i >= min_received_kit_for_unwarranted * (tez_i / tez_to_auction) <=>
   tez_to_auction * kit_i >= min_received_kit_for_unwarranted * tez_i

Misc
----

-  ``fminting > fliquidation``
-  ``minting_price >= liquidation_price``
-  ``liquidation_penalty = 10%``
