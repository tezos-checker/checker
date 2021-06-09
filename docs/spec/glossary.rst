Glossary
========

Kit
---

A coin / token created and destroyed as part of the system.

Burrow
------

A contract for a “deposit account” that supports a few operations,
e.g. “minting” (ie. borrowing) kit, or “burning” (ie. repaying) kit. A
fresh burrow contract is created for every depositor, and only Checker
is allowed to access it, so operations on burrows are performed via the
Checker contract.

Circulating kits
----------------

The number of kits that exist. See also: `outstanding
kits <#outstanding-kits>`__.

Outstanding kits
----------------

The number of kits that it would take to close all currently open
burrows. See also: `circulating kits <#circulating-kits>`__.

Liquidation lot
---------------

A batch of `liquidation slices <#liquidation-slice>`__ currently being
auctionned of.

Liquidation slice
-----------------

Some amount of tez, tied to a burrow, which is inserted in the
liquidation queue to be auctionned of for `kit <#kit>`__

Liquidation queue
-----------------

A dequeue implemented as a balanced binary tree representing an ordered
list of `liquidation slices <#liquidation-slice>`__. Slices at the front
of the queue are periodically batched into a `liquidation
lot <#liquidation-lot>`__

Imbalance
---------

The ratio of the number of `circulating kits <#circulating-kits>`__ to
the number of `outstanding kits <#outstanding-kits>`__.

Imbalance adjustment
--------------------

A compounding fee or reward applied to `burrows <#burrows>`__ which
implicitly increases or decreases the number of `outstanding
kits <#outstanding-kits>`__ over time to bring it closer to the number
of `circulating kits <#circulating-kits>`__ so as to bring the
`imbalance <#imbalance>`__ closer to 1.
