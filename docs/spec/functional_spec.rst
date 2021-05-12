Functional Specification
########################

Working with burrows
====================

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

Deposit a non-negative amount of tez as collateral to a burrow. Fail if
the burrow does not exist, or if the sender is not the burrow owner.

``deposit_tez: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow in which to deposit the tez              |
+---------------+-----------------------+-------------------------------------------------------------------------+


Withdraw collateral from a burrow
---------------------------------

Withdraw a non-negative amount of tez from a burrow. Fail if the burrow
does not exist, if this action would overburrow it, or if the sender is not
the burrow owner.

``withdraw_tez: (pair mutez nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| amount        | mutez                 | The amount of collateral to withdraw                                    |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The caller's ID for the burrow from which to withdraw the tez           |
+---------------+-----------------------+-------------------------------------------------------------------------+


Mint kit
--------

Mint kits from a specific burrow. Fail if the burrow does not exist, if
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

Deposit/burn a non-negative amount of kit to a burrow. If there is excess
kit, simply store it into the burrow. Fail if the burrow does not exist, or
if the sender is not the burrow owner.

``burn_kit: (pair nat nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow in which to burn the kit                 |
+---------------+-----------------------+-------------------------------------------------------------------------+
| amount        | nat                   | The amount of kit to burn                                               |
+---------------+-----------------------+-------------------------------------------------------------------------+


Deactivate a burrow
-------------------

Deactivate a currently active burrow. Fails if the burrow does not exist,
if it is already inactive, if it is overburrowed, if it has kit
outstanding, if it has collateral sent off to auctions, or if the sender is
not the burrow owner. If deactivation is successful, make a tez payment to
the given address.

``deactivate_burrow: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The caller's ID for the burrow to deactivate                            |
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


Perform burrow maintenance
--------------------------

``touch_burrow: (pair address nat)``



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
| ctez          | nat                   | An amount of ctez to be sold for kit                                    |
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
| ctez          | nat                   | The minimum amount of ctez expected to be bought                        |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+

Provide liquidity
-----------------

Deposit some ctez and kit for liquidity in exchange for receiving
liquidity tokens. If the given amounts do not have the right ratio,
the CFMM contract keeps as much of the given ctez and kit as possible
with the right ratio, and returns the leftovers, along with the
liquidity tokens.

``add_liquidity: (pair (pair nat nat) nat timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctez          | nat                   | The amount of ctez to supply as liquidity                               |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The amount of kit to supply as liquidity                                |
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
| ctez          | nat                   | The minimum amount of ctez expected                                     |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The minimum amount of kit expected                                      |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Liquidation Auctions
====================

Mark a burrow for liquidation
-----------------------------

``mark_for_liquidation: (pair address nat)``

Process completed liquidation slices
------------------------------------

``touch_liquidation_slices: (list int)``

Cancel pending liquidation slices
---------------------------------

``cancel_liquidation_slice: int``

Bid in the current liquidation auction
--------------------------------------

``liquidation_auction_place_bid: nat``

Claim the collateral from a winning auction bid
-----------------------------------------------

``liquidation_auction_claim_win: int``

Gather won collateral for a subsequent claim
--------------------------------------------

``receive_slice_from_burrow: (pair address nat)``

Maintenance entrypoints
=======================

Perform Checker internal maintenance
------------------------------------

``touch: unit``

Apply an Oracle update
----------------------

``receive_price: nat``

FA1.2 Interface
===============

Query balance
-------------

::

    balance_of: (pair (list %requests (pair (address %owner) (nat %token_id)))
                      (contract %callback
                         (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance)))))``

Update operators
----------------

::

     update_operators: (list (or (pair %add_operator (address %owner) (address %operator) (nat %token_id))
                                (pair %remove_operator (address %owner) (address %operator) (nat %token_id))))


Deployment
==========

Deploy a lazy function
----------------------

Prior to sealing, the bytecode for each lazy function must be deployed.

``deployFunction: (pair int bytes)``

Seal the contract and make it ready for use
-------------------------------------------

``sealContract: (pair address address)``