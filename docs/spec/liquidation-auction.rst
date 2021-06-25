Liquidation Auctions
====================

State
-----

-  ``avl_storage``: data structure containing all auctions and their corresponding liquidation slices.
   Items can be retrieved from this back-end using their respective pointers.
-  ``queued_slices``: a pointer to the queue of liquidation slices awaiting inclusion in an auction.
-  ``current_auction``: information about the current auction if there
   is an active auction.

   -  ``contents``: a pointer to the set of slices in the current auction.
   -  ``auction_state``: whether the auction is in the descending or
      ascending phase, and data used to calculate the current price.

-  ``completed_auctions``: a queue (represented as a doubly-linked list) of completed auctions, each auction
   containing:

   -  a set of untouched slices
   -  the result of an auction, containing the amount of tez sold,
      amount of kit gained and the winner of an auction.

At any point in time, any liquidation slice is in only one of the above
sets, and they always move from ``queued_slices``, to
``current_auction`` and then to ``completed_auctions``, (then they
disappear). Additionally, this move always happens in order, so an older
liquidation_slice is always further in the process than a younger one.

NOTE: *Per-burrow liquidation_slices* We need to have access to the
liquidation slices for a specific burrow; so slices for a burrow form a
doubly-linked list, each burrow storing a pair of pointers called
``liquidation_slices``, pointing to the first and the last liquidation
slice of that burrow (if they exist).

See <./avl_diagram.drawio> file for an illustration.

Initiating a liquidation
------------------------

When liquidation of a burrow is triggered, the amount of tez to be
liquidated form a ``liquidation_slice``. \* For details about this
process, see <./burrow-state-liquidations.md>.

The new slice is added to the back of the ``queued_slices`` queue.

-  NOTE: This operation also updates the per-burrow linked list.

Cancelling a liquidation slice
------------------------------

Burrows can cancel auctioning off their liquidation slices on certain
conditions. When cancelling a slice, we check if the slice belongs to
the ``queued_slices``, if so, remove it from the set (returning contents
back to the burrow). If not, the process fails.

-  NOTE: This operation also updates the per-burrow linked list.
-  NOTE: This requires a the queue to have an efficient membership test.
-  NOTE: This requires a the queue to support efficient random deletes.

Lot auction
-----------

At any time checker is touched, when there is no auction running and
there is at least one queued slice, we start an auction.

Our aim is to take a prefix of the ``queued_slices`` queue which
contains exactly this amount of tez:

::

   min
     total_queued_tez
     (max
       Constant.max_lot_size
       (total_queued_tez * Constants.min_lot_auction_queue_fraction))

However, it is likely that in this process the slices will not add up to
the exact amount. In this case, we take the liquidation_slice causing
the overload, split it into two, and push the halves to the end of the
new auction and in front of the ``queued_slices``.

-  NOTE: This splitting process has to be efficient, since a single
   auction likely consists of many small slices. So it needs to be done
   without traversing the entire prefix. This pretty much forces us to
   use a tree-like structure with branches containing the aggregate tez
   information of their sub-trees.

Then we start an auction. An auction has minimum_bid value that is a
function of current time and the latest bid.

Every bid should be of at least ``minimum_bid`` amount of kit. The bidding
process debits the bid's kit from the contract's kit ledger and credits back
the kit of the previously winning bid if one exists.

The auction is initially a **descending** auction, with the minimum bid
calculated as:

::

   amount_of_tez_inside_auction
     * tz_minting
     * q
     * ((1 - Constants.auction_decay_rate) ^ time_elapsed_since_auction_start)

After the first bid, it becomes an **ascending** auction, with the
minimum bid calculated as:

::

   leading_bid * (1+Constants.bid_improvement_factor)

The auction finishes when the longer of 20 blocks or 20 minutes are
passed after the last bid.

Touching a liquidation_slice
----------------------------

“Touching the liquidation slice” is the process of propagating the
result of a completed auction back to the burrows. When it is triggered,
we:

1. Check if the given slice belongs to a completed auction, ignore
   otherwise.
2. Remove the slice from the contents of the relevant completed_auction.
3. Remove the slice from the linked list at relevant burrows
   ``liquidation_slices``.

Claiming a winning bid
----------------------

If a bid is the winning bid of a completed auction where all the
liquidation_slices are touched (in other words, its contents are empty), the
bidder can claim the auction's winnings. This process is the final step of an
auction, and after that the auction itself is cleaned up.

Maintenance
-----------

Every time the main checker contract is touched, it touches
``Constants.number_of_slices_to_process`` amount of oldest
liquidation_slice’s automatically.
