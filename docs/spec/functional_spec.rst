Functional Specification
########################

Working with burrows
====================

Create a burrow
---------------

``Create_burrow of Ligo.key_hash option``

Deposit collateral in a burrow
------------------------------

``Deposit_tez of CheckerTypes.burrow_id``

+---------------+------------+----------------------------------------------------------+
| Parameter     | Field Type | Description                                              |
+===============+============+==========================================================+
| burrow_id     | string     | The ID of the burrow in which to deposit collateral      |
+---------------+------------+----------------------------------------------------------+


Withdraw collateral from a burrow
---------------------------------

``Withdraw_tez of Ligo.tez * CheckerTypes.burrow_id``

Mint kit
--------

``Mint_kit of CheckerTypes.burrow_id * Kit.kit``

Burn kit
--------

``Burn_kit of CheckerTypes.burrow_id * Kit.kit``

Deactivate a burrow
-------------------

``Deactivate_burrow of CheckerTypes.burrow_id``

Activate an inactive burrow
---------------------------

``Activate_burrow of CheckerTypes.burrow_id``

Perform burrow maintenance
--------------------------

``Touch_burrow of CheckerTypes.burrow_id``

Set the delegate for a burrow
-----------------------------

``Set_burrow_delegate of CheckerTypes.burrow_id * Ligo.key_hash option``

CFMM Exchange
=============

Buy kit using ctez
------------------

``Buy_kit of Ctez.ctez * Kit.kit * Ligo.timestamp``

Sell kit for ctez
-----------------

``Sell_kit of Kit.kit * Ctez.ctez * Ligo.timestamp``

Provide liquidity
-----------------

``Add_liquidity of Ctez.ctez * Kit.kit * Ligo.nat * Ligo.timestamp``

Withdraw liquidity
------------------

``Remove_liquidity of Ligo.nat * Ctez.ctez * Kit.kit * Ligo.timestamp``

Liquidation Auctions
====================

Mark a burrow for liquidation
-----------------------------

``Mark_for_liquidation of CheckerTypes.burrow_id``

Process completed liquidation slices
------------------------------------

``Touch_liquidation_slices of LiquidationAuctionPrimitiveTypes.leaf_ptr list``

Cancel pending liquidation slices
---------------------------------

``Cancel_liquidation_slice of LiquidationAuctionPrimitiveTypes.leaf_ptr``

Bid in the current liquidation auction
--------------------------------------

``Liquidation_auction_place_bid of Kit.kit``

Claim the collateral from a winning auction bid
-----------------------------------------------

``Liquidation_auction_claim_win of LiquidationAuctionPrimitiveTypes.liquidation_auction_id``

Gather won collateral for a subsequent claim
--------------------------------------------

``Receive_slice_from_burrow of unit``

Maintenance entrypoints
=======================

Perform Checker internal maintenance
------------------------------------

``Touch of unit``

Apply an Oracle update
----------------------

``Receive_price of Ligo.nat``

FA1.2 Interface
===============

``Update_operators of Fa2Interface.fa2_update_operator list``
