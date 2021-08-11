Functional Specification
########################

Working with burrows
====================

Burrows are implicitly associated with their owner via the caller's
address. A caller can operate multiple burrows over time: owners are
expected to identify each burrow uniquely with an arbitrary numeric ID
they supply. These numbers need not be contiguous.

Create a burrow
---------------

Create and return a new burrow containing the supplied tez as collateral,
minus the creation deposit. Fail if the tez is not enough to cover the
creation deposit.

``create_burrow: (pair nat (option key_hash))``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | An arbitrary number to identify the burrow among the caller's burrows   |
+---------------+-----------------------+-------------------------------------------------------------------------+
| delegate      | option key_hash       | An optional delegate for the created burrow contract                    |
+---------------+-----------------------+-------------------------------------------------------------------------+


Deposit collateral in a burrow
------------------------------

Deposit an amount of tez as collateral to a burrow. Fail if
the burrow does not exist, or if the sender is not the burrow owner.

``deposit_collateral: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow in which to deposit the tez              |
+---------------+-----------------------+-------------------------------------------------------------------------+


Withdraw collateral from a burrow
---------------------------------

Withdraw an amount of tez from a burrow. Fail if the burrow
does not exist, if this action would overburrow it, or if the sender is not
the burrow owner.

``withdraw_collateral: (pair nat mutez)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow from which to withdraw the tez           |
+---------------+-----------------------+-------------------------------------------------------------------------+
| amount        | mutez                 | The amount of collateral to withdraw                                    |
+---------------+-----------------------+-------------------------------------------------------------------------+


Mint kit
--------

Mint an amount of kit from a specific burrow. Fail if the burrow does not exist, if
there is not enough collateral, or if the sender is not the burrow owner.

``mint_kit: (pair nat nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow in which to mint the kit                 |
+---------------+-----------------------+-------------------------------------------------------------------------+
| amount        | nat                   | The amount of kit to mint                                               |
+---------------+-----------------------+-------------------------------------------------------------------------+


Burn kit
--------

Deposit/burn an amount of kit to a burrow. If there is excess kit,
simply credit it back to the burrow owner. Fail if the burrow does not exist,
or if the sender is not the burrow owner.

``burn_kit: (pair nat nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow in which to burn the kit                 |
+---------------+-----------------------+-------------------------------------------------------------------------+
| amount        | nat                   | The amount of kit to burn                                               |
+---------------+-----------------------+-------------------------------------------------------------------------+


Activate an inactive burrow
---------------------------

Activate a currently inactive burrow. Fail if the burrow does not exist,
if the burrow is already active, if the amount of tez given is less than
the creation deposit, or if the sender is not the burrow owner.

``activate_burrow: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow to activate                              |
+---------------+-----------------------+-------------------------------------------------------------------------+


Deactivate a burrow
-------------------

Deactivate a currently active burrow. Fails if the burrow does not exist,
if it is already inactive, if it is overburrowed, if it has kit
outstanding, if it has collateral sent off to auctions, or if the sender is
not the burrow owner. If deactivation is successful, make a tez payment to
the given address.

``deactivate_burrow: (pair nat address)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow to deactivate                            |
+---------------+-----------------------+-------------------------------------------------------------------------+
| receiver      | address               | The address to send the burrow's collateral and creation deposit to     |
+---------------+-----------------------+-------------------------------------------------------------------------+


Perform burrow maintenance
--------------------------

Perform maintenance tasks on a burrow (i.e., update it's outstanding kit
according to the system changes that have taken place since the last time the
burrow was operated on). Fails if the burrow does not exist.

``touch_burrow: (pair address nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The caller's ID for the burrow to deactivate                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Set the delegate for a burrow
-----------------------------

Set the delegate of a burrow. Fail if if the sender is not the burrow
owner.

``set_burrow_delegate: (pair nat (option key_hash))``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow                                          |
+---------------+-----------------------+-------------------------------------------------------------------------+
| delegate      | option key_hash       | The key_hash of the new delegate's address, or none                     |
+---------------+-----------------------+-------------------------------------------------------------------------+


CFMM Exchange
=============

Buy kit using ctez
------------------

Buy some kit from the CFMM contract in exchange for ctez. Fail if the
desired amount of kit cannot be bought or if the deadline has passed.

``buy_kit: (pair (pair nat nat) timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctez          | nat                   | An amount of ctez to be sold for kit, in muctez                         |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The minimum amount of kit expected to be bought                         |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Sell kit for ctez
-----------------

Sell some kit in exchange for ctez. Fail if the desired amount of ctez
cannot be bought or if the deadline has passed.

``sell_kit: (pair (pair nat nat) timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| kit           | nat                   | The amount of kit to be sold                                            |
+---------------+-----------------------+-------------------------------------------------------------------------+
| ctez          | nat                   | The minimum amount of ctez expected to be bought, in muctez             |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+

Provide liquidity
-----------------

Deposit some ctez and kit for liquidity in exchange for receiving
liquidity tokens. If the given amounts do not have the right ratio,
the CFMM contract keeps all the ctez given and as much of the given kit as
possible with the right ratio, and returns the leftovers, along with the
liquidity tokens.

``add_liquidity: (pair (pair nat nat) nat timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctez          | nat                   | The amount of ctez to supply as liquidity, in muctez                    |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The maximum amount of kit to supply as liquidity                        |
+---------------+-----------------------+-------------------------------------------------------------------------+
| min_tokens    | nat                   | The minimum number of liquidity tokens expected to be bought            |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Withdraw liquidity
------------------

Redeem some liquidity tokens in exchange for ctez and kit in the right
ratio.

``remove_liquidity: (pair (pair nat nat) nat timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| amount        | nat                   | The number of liquidity tokens to redeem                                |
+---------------+-----------------------+-------------------------------------------------------------------------+
| ctez          | nat                   | The minimum amount of ctez expected, in muctez                          |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The minimum amount of kit expected                                      |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Liquidation Auctions
====================

Mark a burrow for liquidation
-----------------------------

Mark a burrow for liquidation. Fails if the the burrow does not exist or if it
is not a candidate for liquidation. If the operation is successful, a tez
payment is made to Tezos.sender with the liquidation reward.

``mark_for_liquidation: (pair address nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The caller's ID for the burrow to mark for liquidation                  |
+---------------+-----------------------+-------------------------------------------------------------------------+


Process completed liquidation slices
------------------------------------

Process a number of liquidation slices (i.e., amounts of tez that have been
auctioned off as part of completed liquidation auctions). Fails if any of the
identifiers given are not valid. Pointers to slices of incomplete auctions are
ignored.

``touch_liquidation_slices: (list int)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| slice_ptrs    | list int              | The unique identifiers of the slices to be processed                    |
+---------------+-----------------------+-------------------------------------------------------------------------+


Cancel pending liquidation slices
---------------------------------

Cancel the liquidation of a liquidation slice. Fails if the sender is not the
burrow owner, if the slice is part of an ongoing or completed auction, or if
the burrow is currently overburrowed.

``cancel_liquidation_slice: int``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| slice_ptr     | int                   | The unique identifier of the slice whose liquidation is to be cancelled |
+---------------+-----------------------+-------------------------------------------------------------------------+


Bid in the current liquidation auction
--------------------------------------

Bid in the current liquidation auction. Fails if there is no ongoing auction or
if the bid is too low.

``liquidation_auction_place_bid: (pair nat nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| auction_id    | nat                   | The unique identifier of the currently ongoing liquidation auction      |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The amount of kit to be bid                                             |
+---------------+-----------------------+-------------------------------------------------------------------------+


Claim the collateral from a winning auction bid
-----------------------------------------------

Claim the rewards of a completed liquidation auction. Fails if the sender is
not the auction winner, if the auction is still ongoing, or if the completed
auction still has unprocessed liquidation slices. If the operation is
successful, a tez payment is made to Tezos.sender with the auction winnings.

``liquidation_auction_claim_win: int``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| auction_id    | int                   | The unique identifier of the completed auction                          |
+---------------+-----------------------+-------------------------------------------------------------------------+


Gather won collateral for a subsequent claim
--------------------------------------------

Internal. Receive a liquidation slice (tez) from a burrow.

``receive_slice_from_burrow: (pair address nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The caller's ID for the burrow sending the slice                        |
+---------------+-----------------------+-------------------------------------------------------------------------+


Maintenance entrypoints
=======================

Perform Checker internal maintenance
------------------------------------

Perform housekeeping tasks on the contract state. This includes:
* updating the system parameters;
* accruing burrowing fees to the cfmm;
* updating auction-related info (completing an old / starting a new auction);
* processing a limited number of liquidation slices from completed auctions;
* updating the index by consulting the oracle.

This operation credits an amount of kit (that is a function of time passed
since the last time ``touch`` was called) to Tezos.sender.

``touch: unit``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| unit          | unit                  | ()                                                                      |
+---------------+-----------------------+-------------------------------------------------------------------------+


Apply an Oracle update
----------------------

Internal. Receive a price update from the registered oracle.

``receive_price: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| price         | nat                   | The current index, as a fixedpoint with a scaling factor of 1000000     |
+---------------+-----------------------+-------------------------------------------------------------------------+


FA1.2 Interface
===============

Query balance
-------------

::

    balance_of: (pair (list %requests (pair (address %owner) (nat %token_id)))
                      (contract %callback
                         (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance)))))

Update operators
----------------

::

     update_operators: (list (or (pair %add_operator (address %owner) (address %operator) (nat %token_id))
                                (pair %remove_operator (address %owner) (address %operator) (nat %token_id))))


FA2 Views
=========

Checker exposes a number of FA2 views in its contract
metadata. Standard token views are provided, as are a number of custom
views provided for integration convenience, e.g. for use by front-end
applications.

Standard FA2 views
------------------

The following standard FA2 views are supported:

* ``get_balance``
* ``total_supply``
* ``all_tokens``
* ``is_operator``


Estimate yield when buying kit with ctez
----------------------------------------

Get the maximum amount of kit that can be expected to be received for the given
amount of ctez (when calling ``buy_kit``), based on the current market price.

``buy_kit_min_kit_expected : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctez          | nat                   | The amount of ctez to be sold to the cfmm                               |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate yield when selling kit for ctez
----------------------------------------

Get the maximum amount of ctez that can be expected to be received for the
given amount of kit (when calling ``sell_kit``), based on the current market
price.

``sell_kit_min_ctez_expected : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| kit           | nat                   | The amount of kit to be sold to the cfmm                                |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate kit requirements when adding liquidity
-----------------------------------------------

Get the minimum amount of kit that needs to be deposited when adding liquidity
for the given amount of ctez (when calling ``add_liquidity``), based on the
current market price.

``add_liquidity_max_kit_deposited : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctez          | nat                   | The amount of ctez to be given as liquidity                             |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate yield when adding liquidity
------------------------------------

Get the maximum amount of the liquidity token that can be expected to be
received for the given amount of ctez (when calling ``add_liquidity``), based
on the current market price.

``add_liquidity_min_lqt_minted : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctez          | nat                   | The amount of ctez to be given as liquidity                             |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate ctez yield when removing liquidity
-------------------------------------------

Get the maximum amount of ctez that can be expected to be received for the
given amount of liquidity token (when calling ``remove_liquidity``), based on
the current market price.

``remove_liquidity_min_ctez_withdrawn : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| liquidity     | nat                   | The amount of liquidity token to be returned to the cfmm                |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate kit yield when removing liquidity
------------------------------------------

Get the maximum amount of kit that can be expected to be received for the given
amount of liquidity token (when calling ``remove_liquidity``), based on the
current market price.

``remove_liquidity_min_kit_withdrawn : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| liquidity     | nat                   | The amount of liquidity token to be returned to the cfmm                |
+---------------+-----------------------+-------------------------------------------------------------------------+


Find maximum kit that can be minted
-----------------------------------

``burrow_max_mintable_kit : nat -> nat``

Returns the maximum amount of kit that can be minted from the given burrow.

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow                                          |
+---------------+-----------------------+-------------------------------------------------------------------------+


Check whether a burrow is overburrowed
--------------------------------------

``is_burrow_overburrowed : (pair address nat) -> bool``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The caller's ID for the burrow in question                              |
+---------------+-----------------------+-------------------------------------------------------------------------+


Check whether a burrow can be liquidated
----------------------------------------

``is_burrow_liquidatable : nat -> bool``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow                                          |
+---------------+-----------------------+-------------------------------------------------------------------------+


Minimum bid for the current liquidation auction (if exists)
-----------------------------------------------------------

``current_liquidation_auction_minimum_bid : unit -> pair nat nat``

Returns a pair of an identifier to the current auction and an amount of kit.

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| unit          | unit                  | ()                                                                      |
+---------------+-----------------------+-------------------------------------------------------------------------+


Deployment
==========

Deploy a lazy function
----------------------

Prior to sealing, the bytecode for each lazy function must be deployed.

``deployFunction: (pair int bytes)``

Deploy metadata
---------------

Prior to sealing, the bytecode for all metadata must be deployed.

``deployMetadata: bytes``

Seal the contract and make it ready for use
-------------------------------------------

``sealContract: (pair address address)``
