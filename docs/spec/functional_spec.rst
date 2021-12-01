Functional Specification
########################

Checker
*******

Working with burrows
====================

Burrows are implicitly associated with their owner via the sender's address
upon their creation. A sender can operate multiple burrows over time: owners
are expected to identify each burrow uniquely with an arbitrary numeric ID they
supply. These numbers need not be contiguous.

Create a burrow
---------------

Create and return a new burrow containing the supplied amount of FA2 token as
collateral, minus the creation deposit. Fails if the collateral given is not
enough to cover the creation deposit, if the sender does not own said amount of
collateral, or if Checker is not authorized as an operator for the sender's collateral.

``create_burrow: (pair (pair nat (option key_hash)) nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | An arbitrary number to identify the burrow among the sender's burrows   |
+---------------+-----------------------+-------------------------------------------------------------------------+
| delegate      | option key_hash       | An optional delegate for the created burrow contract                    |
+---------------+-----------------------+-------------------------------------------------------------------------+
| tok           | nat                   | The amount of token supplied as collateral (including creation deposit) |
+---------------+-----------------------+-------------------------------------------------------------------------+


Deposit collateral in a burrow
------------------------------

Deposit an amount of FA2 token as collateral to a burrow. Fails if the burrow does
not exist, if the sender does not own said collateral, or if Checker is not
authorized as an operator for the sender's collateral.

``deposit_collateral: (pair nat nat)``

+---------------+-----------------------+---------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                               |
+===============+=======================+===========================================================================+
| id            | nat                   | The sender's ID for the burrow in which to deposit the collateral tokens  |
+---------------+-----------------------+---------------------------------------------------------------------------+
| tok           | nat                   | The amount of token supplied to be used as collateral                     |
+---------------+-----------------------+---------------------------------------------------------------------------+


Withdraw collateral from a burrow
---------------------------------

Withdraw an amount of collateral from a burrow. Fails if the burrow does not
exist, if this action would overburrow it, or if the sender is not the burrow
owner.

``withdraw_collateral: (pair nat nat)``

+---------------+-----------------------+-----------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                                 |
+===============+=======================+=============================================================================+
| id            | nat                   | The sender's ID for the burrow from which to withdraw the collateral tokens |
+---------------+-----------------------+-----------------------------------------------------------------------------+
| amount        | nat                   | The amount of collateral to withdraw                                        |
+---------------+-----------------------+-----------------------------------------------------------------------------+


Mint kit
--------

Mint an amount of kit from a specific burrow. Fails if the burrow does not
exist, if there is not enough collateral, or if the sender is not the burrow
owner.

``mint_kit: (pair nat nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The sender's ID for the burrow in which to mint the kit                 |
+---------------+-----------------------+-------------------------------------------------------------------------+
| amount        | nat                   | The amount of kit to mint                                               |
+---------------+-----------------------+-------------------------------------------------------------------------+


Burn kit
--------

Deposit/burn an amount of kit to a burrow. If there is excess kit, simply
credit it back to the burrow owner. Fails if the sender does not own the
specified amount of kit, if the burrow does not exist, or if the sender is not
the burrow owner.

``burn_kit: (pair nat nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The sender's ID for the burrow in which to burn the kit                 |
+---------------+-----------------------+-------------------------------------------------------------------------+
| amount        | nat                   | The amount of kit to burn                                               |
+---------------+-----------------------+-------------------------------------------------------------------------+


Activate an inactive burrow
---------------------------

Activate a currently inactive burrow. Fails if the burrow does not exist, if the
burrow is already active, if the amount of collateral given is not enough to
cover the creation deposit, if the sender does not own said collateral, or if
Checker is not authorized as an operator for the sender's collateral.

``activate_burrow: (pair nat nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The sender's ID for the burrow to activate                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| tok           | nat                   | The amount of token supplied as collateral (including creation deposit) |
+---------------+-----------------------+-------------------------------------------------------------------------+


Deactivate a burrow
-------------------

Deactivate a currently active burrow. Fails if the burrow does not exist, if it
is already inactive, if it is overburrowed, if it has kit outstanding, or if it
has collateral sent off to auctions. If deactivation is successful, emits an
FA2 transfer to the given address.

``deactivate_burrow: (pair nat address)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The sender's ID for the burrow to deactivate                            |
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
| id            | nat                   | The sender's ID for the burrow to deactivate                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Set the delegate for a burrow
-----------------------------

Set the delegate of a burrow. Fails if if the sender is not the burrow owner or
if the deployed checker instance does not use tez as collateral.

``set_burrow_delegate: (pair nat (option key_hash))``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| id            | nat                   | The sender's ID for the burrow                                          |
+---------------+-----------------------+-------------------------------------------------------------------------+
| delegate      | option key_hash       | The key_hash of the new delegate's address, or none                     |
+---------------+-----------------------+-------------------------------------------------------------------------+


CFMM Exchange
=============

Buy kit using cfmm token
------------------------

Buy some kit from the CFMM contract in exchange for the FA2 token used in the cfmm. Fails if
Checker is not authorized as an operator for the sender's cfmm token, if the desired amount
of kit cannot be bought, or if the deadline has passed.

``buy_kit: (pair (pair nat nat) timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctok          | nat                   | An amount of cfmm token to be sold for kit                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The minimum amount of kit expected to be bought                         |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Sell kit for cfmm token
-----------------------

Sell some kit in exchange for the FA2 token used in the cfmm. Fails if the
sender does not own the specified amount of kit, if the desired amount of cfmm
token cannot be bought, or if the deadline has passed.

``sell_kit: (pair (pair nat nat) timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| kit           | nat                   | The amount of kit to be sold                                            |
+---------------+-----------------------+-------------------------------------------------------------------------+
| ctok          | nat                   | The minimum amount of cfmm token expected to be bought                  |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+

Provide liquidity
-----------------

Deposit some cfmm token and kit for liquidity in exchange for receiving liquidity
tokens. If the given amounts do not have the right ratio, the CFMM contract
keeps all the cfmm token given and as much of the given kit as possible with the
right ratio, and returns the leftovers, along with the liquidity tokens. Fails if
Checker is not authorized as an operator for the sender's cfmm token, or if
the sender does not own the specified amount of kit.

``add_liquidity: (pair (pair nat nat) nat timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctok          | nat                   | The amount of cfmm token to supply as liquidity                         |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The maximum amount of kit to supply as liquidity                        |
+---------------+-----------------------+-------------------------------------------------------------------------+
| min_tokens    | nat                   | The minimum number of liquidity tokens expected to be bought            |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Withdraw liquidity
------------------

Redeem some liquidity tokens in exchange for cfmm tokens and kit in the right ratio.
Fails if the sender does not own the specified liquidity tokens.

``remove_liquidity: (pair (pair nat nat) nat timestamp)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| amount        | nat                   | The number of liquidity tokens to redeem                                |
+---------------+-----------------------+-------------------------------------------------------------------------+
| ctok          | nat                   | The minimum amount of cfmm token expected                               |
+---------------+-----------------------+-------------------------------------------------------------------------+
| kit           | nat                   | The minimum amount of kit expected                                      |
+---------------+-----------------------+-------------------------------------------------------------------------+
| deadline      | timestamp             | The deadline for the transaction to be valid                            |
+---------------+-----------------------+-------------------------------------------------------------------------+


Liquidation Auctions
====================

Mark a burrow for liquidation
-----------------------------

Mark a burrow for liquidation. Fails if the burrow does not exist, or if it is
not a candidate for liquidation. If the operation is successful, a payment is
made to ``Tezos.sender`` with the liquidation reward.

``mark_for_liquidation: (pair address nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The sender's ID for the burrow to mark for liquidation                  |
+---------------+-----------------------+-------------------------------------------------------------------------+


Process completed liquidation slices
------------------------------------

Process a number of liquidation slices (i.e., amounts of collateral that have been
auctioned off as part of completed liquidation auctions). Fails if any of the
identifiers given are not valid. Pointers to slices of incomplete auctions are
ignored.

``touch_liquidation_slices: (list nat)``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| slice_ptrs    | list nat              | The unique identifiers of the slices to be processed                    |
+---------------+-----------------------+-------------------------------------------------------------------------+


Cancel pending liquidation slices
---------------------------------

Cancel the liquidation of a liquidation slice. Fails if the sender is not the
burrow owner, if the slice is part of an ongoing or completed auction, or if
the burrow is currently overburrowed.

``cancel_liquidation_slice: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| slice_ptr     | nat                   | The unique identifier of the slice whose liquidation is to be cancelled |
+---------------+-----------------------+-------------------------------------------------------------------------+


Bid in the current liquidation auction
--------------------------------------

Bid in the current liquidation auction. Fails if the sender does not own the
specified amount of kit, if there is no ongoing auction, or if the bid is too
low.

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
successful, an FA2 transfer of the collateral is made to ``Tezos.sender`` with the auction
winnings.

``liquidation_auction_claim_win: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| auction_id    | nat                   | The unique identifier of the completed auction                          |
+---------------+-----------------------+-------------------------------------------------------------------------+


Maintenance entrypoints
=======================

Perform Checker internal maintenance
------------------------------------

Perform housekeeping tasks on the contract state. This includes:

#. updating the system parameters;
#. accruing burrowing fees to the cfmm;
#. updating auction-related info (completing an old / starting a new auction);
#. processing a limited number of liquidation slices from completed auctions;
#. updating the index by consulting the oracle.

This operation credits an amount of kit (that is a function of time passed
since the last time ``touch`` was called) to ``Tezos.sender``.

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


FA2 Interface
=============

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

Checker exposes a number of FA2 views in its contract metadata. Standard token
views are provided, as are a number of custom views provided for integration
convenience, e.g. for use by front-end applications.

Standard FA2 views
------------------

The following standard FA2 views are supported:

* ``get_balance``
* ``total_supply``
* ``all_tokens``
* ``is_operator``


Estimate yield when buying kit with cfmm tokens
-----------------------------------------------

Get the maximum amount of kit that can be expected to be received for the given
amount of cfmm token (when calling ``buy_kit``), based on the current market price.

``buy_kit_min_kit_expected : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctok          | nat                   | The amount of cfmm token to be sold to the cfmm                         |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate yield when selling kit for cfmm tokens
-----------------------------------------------

Get the maximum amount of cfmm token that can be expected to be received for the
given amount of kit (when calling ``sell_kit``), based on the current market
price.

``sell_kit_min_ctok_expected : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| kit           | nat                   | The amount of kit to be sold to the cfmm                                |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate kit requirements when adding liquidity
-----------------------------------------------

Get the minimum amount of kit that needs to be deposited when adding liquidity
for the given amount of cfmm token (when calling ``add_liquidity``), based on the
current market price.

``add_liquidity_max_kit_deposited : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctok          | nat                   | The amount of cfmm token to be given as liquidity                       |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate yield when adding liquidity
------------------------------------

Get the maximum amount of the liquidity token that can be expected to be
received for the given amount of cfmm token (when calling ``add_liquidity``), based
on the current market price.

``add_liquidity_min_lqt_minted : nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| ctok          | nat                   | The amount of cfmm token to be given as liquidity                       |
+---------------+-----------------------+-------------------------------------------------------------------------+


Estimate cfmm token yield when removing liquidity
-------------------------------------------

Get the maximum amount of cfmm token that can be expected to be received for the
given amount of liquidity token (when calling ``remove_liquidity``), based on
the current market price.

``remove_liquidity_min_ctok_withdrawn : nat -> nat``

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

Returns the maximum amount of kit that can be minted from the given burrow.

``burrow_max_mintable_kit : pair address nat -> nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The sender's ID for the burrow in question                              |
+---------------+-----------------------+-------------------------------------------------------------------------+


Check whether a burrow is overburrowed
--------------------------------------

``is_burrow_overburrowed : pair address nat -> bool``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The sender's ID for the burrow in question                              |
+---------------+-----------------------+-------------------------------------------------------------------------+


Check whether a burrow can be liquidated
----------------------------------------

``is_burrow_liquidatable : pair address nat -> bool``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| owner         | address               | The burrow owner's address                                              |
+---------------+-----------------------+-------------------------------------------------------------------------+
| id            | nat                   | The sender's ID for the burrow in question                              |
+---------------+-----------------------+-------------------------------------------------------------------------+


Get details on the current liquidation auction
----------------------------------------------

Fails if there is currently no liquidation auction.

``current_liquidation_auction_details: unit -> view_current_liquidation_auction_details_result``

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

``sealContract: (pair (pair (pair address address) address) address)``

wtez
****

Overview
========

wtez is a wrapper contract which issues ``wtez`` FA2 tokens which are always equal in
value to ``tez``. It is designed for use cases where users wish to use ``tez`` collateral
in Checker. In this case Checker deals with the ``wtez`` FA2 tokens instead of
with ``tez`` directly.

Each account can have up to exactly one vault contract associated with it which holds
the tez deposited from that account.

Deposits and withdrawals
========================

Deposit tez
-----------

Deposit the amount of ``tez`` in the transaction to the sender's vault in
exhange for ``wtez`` tokens. If the account does not already have a vault
contract an operation will be emitted originating it.

``deposit: unit``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| unit          | unit                  | ()                                                                      |
+---------------+-----------------------+-------------------------------------------------------------------------+

Withdraw tez
------------

Withdraw the specified amount of ``tez`` from the sender's vault in exhange for
``wtez`` tokens. Fails if ``amount`` is greater than the sender's ``wtez``
balance. If the account does not already have a vault contract an operation will
be emitted originating it.

``withdraw: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| amount        | nat                   | The amount of tez to withdraw                                           |
+---------------+-----------------------+-------------------------------------------------------------------------+

Set the delegate for a vault
----------------------------

Set the delegate of the sender's vault. If the account does not already have a
vault contract an operation will be emitted originating it.

``set_delegate: (pair nat (option key_hash))``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| delegate      | option key_hash       | The key_hash of the new delegate's address, or none                     |
+---------------+-----------------------+-------------------------------------------------------------------------+


FA2 Interface
=============

# TODO: fa2 entrypoints

``balance_of``

``transfer``

``update_operators``

Internal entrypoints
====================

# TODO: callVaultReceiveTez

``call_vault_receive_tez``

``call_vault_send_tez_to_contract``

``call_vault_send_tez_to_vault``

``call_vault_set_delegate``

wctez
*****

Overview
========

wctez is a wrapper contract which issues ``wctez`` FA2 tokens in exhange for
``ctez`` tokens. It was designed for use in Checker's CFMM which only works with
FA2 tokens since ``ctez`` itself only provides an FA1.2 interface.

Each ``wctez`` token is always worth exactly one ``ctez`` token.

Minting and redeeming tokens
============================

Minting tokens
--------------

Mint ``wctez`` tokens by transfering the corresponding amount of ``ctez`` tokens
from the sender to the contract. Fails if the contract is not approved to spend
the specified amount of ``ctez`` tokens on the sender's behalf or if the
sender's ``ctez`` balance is less than the specified amount.

``mint: unit``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| amount        | nat                   | The amount of ctez tokens to transfer to the contract                   |
+---------------+-----------------------+-------------------------------------------------------------------------+

Redeeming tokens
----------------

Redeem the specified amount of ``wctez`` tokens for the same amount of ``ctez``
tokens. Fails if the sender's ``wctez`` balance is less than the specified
amount.

``withdraw: nat``

+---------------+-----------------------+-------------------------------------------------------------------------+
| Parameter     |      Field Type       | Description                                                             |
+===============+=======================+=========================================================================+
| amount        | nat                   | The amount of wctez tokens to redeem                                    |
+---------------+-----------------------+-------------------------------------------------------------------------+


FA2 Interface
=============

# TODO: fa2 entrypoints

``balance_of``

``transfer``

``update_operators``