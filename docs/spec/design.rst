Design
======

This document starts by introducing some important concepts underlying
the notion of this robocoin and its implementation, such as the notion
of target, quantity, and oracles. Then, we present the algorithmic
component that ensures the stability of the target. However, all the
robocoin system with its control mechanism needs kits to be created
and destroyed. This is made possible by the notion of
burrows. Therefore, we present in a subsequent part burrows and their
lifecycle, including creation, auctions, and liquidation. Finally, we
discuss how the automatic control mechanism can be complemented by
on-chain governance, which completes the whole picture of the notion
of robocoin.

Concepts
--------

The following sections define concepts which together work to form
Checker.

The clock
~~~~~~~~~

Any computation on a blockchain happens in discrete time. We note the
timestamps at which the Checker system is updated as a series of
increasing timestamps, :math:`t_i`. Ideally, these updates happen every
time a new block is added to the blockchain and thus, under the current
economic protocol, those timestamps are separated by about a minute
each.

That said, the system is designed to be resilient to changes in the
interblock time and also to occasional missing updates upon added
blocks.

Target and quantity
~~~~~~~~~~~~~~~~~~~

In Checker, robocoins, denominated in “kit” are algorithmically balanced
to achieve a certain degree of steadiness with respect to a **target**,
which is expressed in terms of a **quantity** and an **index**.

The word “kit” is chosen because it’s short, simple to pronounce, and
means a baby fox (which seems appropriate for a smart currency).

The **quantity**, :math:`q_{t_i}`, expressed in
:math:`\mathrm{kit}^{-1}`, is a time-dependent property of the system
which can fluctuate upwards or downwards.

The **index**, expressed in :math:`\mathrm{kit}`, is an external
time-dependent measure of value. Examples of an index include:

-  “the median hourly minimum wage across OCDE countries, expressed in
   kits”, and
-  “the value of one CHF (Swiss Franc), expressed in kits”.

This index is provided through a combination of off-chain and on-chain
oracles.

The **target** :math:`p_{t_i}` is the dimensionless product of the
**index** and the **quantity**. Examples of **target** include:

-  “the median of :math:`q_{t_i}` hours of minimum wage across all OCDE
   countries as a number of kits”,
-  “the minimal compensation, in kits, that an airline might owe a
   passenger, pursuant to the Vienna convention, should they lose
   :math:`q_{t_i}` kg of luggage as a number of kits”, and
-  “how many kits for :math:`q_{t_i}` Swiss Francs”.

Intuitively, if the target :math:`p_{t_i}` is below :math:`1`, then kits
are, in a sense “too expensive” and if the target is above :math:`1`
then kits are, in a sense, “too cheap”.

The instantaneous drift
~~~~~~~~~~~~~~~~~~~~~~~

The **instantaneous drift** :math:`d_{t}`, or just **drift**, is a
dynamic parameter (which varies continuously over time) used for
adjusting the quantity :math:`q`. It represents the growth or shrinking
of :math:`q` per unit of time, and is measured in
`Nepers <https://en.wikipedia.org/wiki/Neper>`__ (Np), or sub-units
thereof, such as centinepers (cNp). The drift is implicitly continuous,
piecewise-quadratic between two clock ticks with continuous derivatives.

The system applies algorithmic control mechanisms in order to produce a
drift that is defined at all time :math:`t`.

Essentially, our control mechanism provides :math:`d'` the derivative of
:math:`d` at clock ticks, and we interpolate quadratically between them.

We can set :math:`d_0=d'_0=0`; the system will adjust automatically so
the initial values do not particularly matter (so long as they are not
absurdly large).

:math:`d'_{t_{i+1}}` is computed as defined in the `algorithmic control
section <#Algorithmic-control>`__ and :math:`d_{t_{i+1}}` is then:

.. math:: d_{t_{i+1}} = d_{t_i} + \frac{1}{2}(d'_{t_i} + d'_{t_{i+1}})(t_{i+1} - t_i)

The drift :math:`d(t)` is applied to the quantity such that
:math:`q'(t) = d(t) q(t)`, hence:

.. math:: q_{t_{i+1}} = q_{t_i} \textrm{exp}\left(\left(d_{t_i} + \frac{1}{6}(2 d'_{t_i}+d'_{t_{i+1}})(t_{i+1}-t_{i})\right)(t_{i+1}-t_i)\right)

The term in the exponential is the product between

-  the time span :math:`(t_{i+1}-t_i)`, and
-  a term
   :math:`\left(d_{t_i} + \frac{1}{6}(2 d'_{t_i}+d'_{t_{i+1}})(t_{i+1}-t_{i})\right)`
   which is the average of the quadratic function over the period.

Note that given the practical constants involved (:math:`d` is typically
on the order of :math:`10^0` to :math:`10^1` cNp / year), the
exponential can be approximated by :math:`exp(x) = 1+x`\ .

Oracles
~~~~~~~

An oracle feed provides the **tez**-denominated value of the external
index (e.g. 1 CHF), which we label :math:`tz_t`. The contract
providing the oracle feed should be reliable: for some external
measures it might be advisable for that contract to give Checker the
median of three or more externally-observed values.

Filtered oracle feeds
^^^^^^^^^^^^^^^^^^^^^

Protected index
'''''''''''''''

The feed of external oracle values is itself filtered.

We define the **protected index**, :math:`\widehat{tz}_t`, as:

.. math:: \widehat{tz}_{t_i} = \widehat{tz}_{t_{i-1}} \times \mathrm{clamp}\!\left(\frac{tz_{t_i}}{\widehat{tz}_{t_{i-1}} }, e^{-\epsilon (t_{i}-t_{i-1})}, e^{\epsilon (t_{i}-t_{i-1})}\right)

where :math:`\mathrm{clamp}\!(x, \mathrm{min}, \mathrm{max})` returns :math:`\mathrm{x}` constrained to the inclusive
range from :math:`\mathrm{min}` to :math:`\mathrm{max}`.

We suggest a value of :math:`\epsilon = 0.05~\mathrm{cNp/min}` – that’s
about 72 cNp / day, so the filter can catch up to a 2x or 0.5x move in
24 hours, and a 3% move in an hour.

:math:`\widehat{tz}_t` is like the suspension of a car, it lags behind
large moves, but is insensitive to spikes (real or fabricated).

In addition, we define the following prices [TODO: mjg suggest creating a
subsection for “Prices” and moving the indexes below down a level. Or
(because they’re quite short), perhaps the below is actually an itemised
list]

Minting index
'''''''''''''

The feed :math:`tz^{minting}_t = \max (tz_t, \widehat{tz}_t)` is the
maximum of :math:`tz_t` and :math:`\widehat{tz}_t`.

Liquidation index
'''''''''''''''''

The feed :math:`tz^{liquidation}_t = \min(tz_t, \widehat{tz}_t)` is the
miminum of :math:`tz_t` and :math:`\widehat{tz_t}`.

Changing oracle feeds
^^^^^^^^^^^^^^^^^^^^^

The set of feeds [TODO: mjg if symbol exists, put it here] is initially fixed.
A Tezos protocol upgrade is **strongly recommended** to give bakers the
ability to signal in each block support for the addition or removal of
oracle feeds. [TODO: mjg I think this may be what is intended:] We **strongly
recommend** that the current Tezos protocol be upgraded to allow bakers
to signal in each block support for adding or removing oracles.

Target
^^^^^^

The Checker system includes a
`uniswap <https://uniswap.org/whitepaper.pdf>`__-like CFMM (Constant
Function Market Maker) exchange contract which gives an indication of
the price of kit in tez, :math:`k_t` with unit
:math:`\mathrm{tez}~\mathrm{kit}^{-1}`. The target can be computed as

.. math:: p_t = q_t tz_t / k_t

For example: suppose

.. math::

   \left\{\begin{array}{ccc}
   tz_t & = & 0.36~\textrm{xtz}\\
   k_t & = & 0.3~\textrm{xtz/kit}\\
   q_t & = & 0.9~\mathrm{kit}^{-1}\end{array}\right.

Then :math:`p_t = 1.08`, and since :math:`p_t > 1`, we can stay that kit
is too cheap.

We do not need to filter the target feed as it only affects the drift in
a bounded way that is, even if :math:`tz_t` experiences wild, short
lived swing, it will not have a major effect on the system.

Algorithmic control
-------------------

Consider the measure of imbalance

.. math:: \log p_t = \log(q_t tz_t / k_t).

All logarithm values are expressed in cNp or centinepers (for small
values, a centineper is almost the same as a percentage point so you can
safely read 2 cNp and 2% as roughly equivalent).

We algorithmically define the drift :math:`d_t` via its rate of change,
noted :math:`d'_t`. :math:`d'_t` is computed, at any clock tick t, based
on the imbalance:

.. math::


   \left\{\begin{array}{ccc}
   |\log p_t| < 0.5~\textrm{cNp} & \Rightarrow & d'_t = 0\\
   0.5~\textrm{cNp} \le |\log p_t| < 5~\textrm{cNp} & \Rightarrow & d'_t = \mathrm{sign}(\log p_t) 0.01~\textrm{cNp}/\textrm{day}^2\\
   5~\textrm{cNp} \le |\log p_t| & \Rightarrow & d'_t = \mathrm{sign}(\log p_t) 0.05~\textrm{cNp}/\textrm{day}^2\\
   \end{array}
   \right.

It’s easy to imagine models where :math:`d'_t` depends continuously on
:math:`\log p_t` but our intuition is that such models tend to be less
robust than simple bang-bang models such as the one above.

Two remarks: 1. The unit of :math:`d_t` is
:math:`\textrm{cNp}/\textrm{day}` because it represents the growth or
shrinking of :math:`q_t` per unit of time. Therefore, it is natural that
the unit of :math:`d'_t` is in :math:`\textrm{cNp}/\textrm{day}^2`. To
get a better intuition of those quantities suppose drift starts at 0 cNp
/ day and imbalance stays below -0.5 cNp but above -5 cNp for a month,
the drift would grow to 0.3 cNp / day, and :math:`q_t` would increase by
4.65 cNp (about 4.76%). If imbalance stayed below 5 cNp for a month, the
drift would go from 0 cNp / day to 1.5 cNp / day in a month, increasing
:math:`q_t` by 23.25 cNp (about 26.18%).

2. When compared to MakerDAO this is essentially setting a rate of
   increase or decrease for a (potentially negative!) stability fee
   programmatically, based on prices, as opposed to votes.

Burrows
-------

Burrows are a form of “deposit account”, and each is an independent
smart contract, originated by the Checker contract.

A burrow serves to hold tez collateral against which kits may be minted
and subsequently burned, subject to certain restrictions. Collateral may
generally be added and withdrawn over time, again subject to
restrictions. Kits minted from a burrow (“outstanding kits”) become part
of the burrow owner’s personal kit balance, and they may be spent or
transferred freely. A corresponding portion of the collateral in the
burrow will then be locked up, and it cannot be withdrawn unless enough
kits are later returned to the burrow and burned. Burrows are similar to
CDPs in MakerDAO.

**Burrow creation deposit**: When a burrow is created, its owner must
pay a burrow creation deposit, which won’t count towards the collateral
and is only there to reward people marking the burrow for liquidation.
If the owner closes the burrow, the deposit is recovered with it. We
propose to set the deposit at 1 tez.

Since the burrow holds tez on the owner’s behalf, the owner may
optionally specify a delegate for that balance.

Burrowing and overburrowing
~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Burrowing** is the act of minting kits out of a burrow, and the kits
accrue to an **outstanding** kits balance. To avoid overpopulation of
kits, the burrowing is limited depending on the number of tez in the
burrow in relation to the outstanding kit balance. Generally, kits can
be minted so long as the tez in the burrow is at least :math:`f^{minting}` times
the number of outstanding kits multiplied by :math:`q_t` multiplied by
:math:`tz^{minting}_t`. We propose :math:`f^{minting} = 2.1`.

Assume for instance :math:`tz^{minting}_t = 0.36 \textrm{xtz}` and
:math:`q_t = 1.015`. To mint 10 kits, one would require
:math:`2.1 \times 10 \times 0.36 \times 1.015 = 7.673~\mathrm{xtz}` in the
burrow. When further kits can no longer be burrowed due to insufficient
tez collateral, the burrow is said to be **overburrowed**.

Even once further minting is blocked due to overburrowing, market
fluctuations in kit and tez values may lead to a situation in which the
ratio of kits outstanding versus tez in the burrow exceeds a higher
safety threshold of :math:`f^{liquidation} q_t tz^{liquidation}_t`, in which case the
burrow is considered under-collateralized and can be marked for
liquidation, as we’ll see later.

Burrow fee
~~~~~~~~~~

While a burrow has outstanding kits, it continuously incurs a
compounding burrow fee. This is an amount added to the outstanding kit
balance, but this amount does not represent kits given to the burrow
owner. The result of this is that over time slightly more kits are
required to be burned in a burrow in order to release its collateral.

A 0.5 cNp fee per year is assessed and implicitly credited to a ctez /
kit CFMM exchange contract which is described below in this document.
It’s important that this is done implicitly, i.e. whenever the CFMM
contract is called, it knows exactly what its balance is.

Note: it might seem at first like the fee is “paid” for, individually,
by the burrow creators but, from an economic perspective, it is equally
valid to view it as being paid for, collectively, by all the kit
holders, as the fee can be offset by an adjustment of the drift.

Imbalance adjustment
~~~~~~~~~~~~~~~~~~~~

The *imbalance adjustment* takes the form of either an *adjustment fee*
or an *adjustment bonus*. The exact amount of the fee (or bonus) is set
depending on the imbalance between the number of kits in circulation and
the outstanding number of kits that would need to be burned to close all
burrows.

In general those numbers should be equal but, imperfect liquidations
could cause the numbers to become different. (Imperfect liquidations
happen when a burrow is completely liquidated, but not all of the
outstanding kits can be recovered: there is an outstanding balance of
kits that were minted out of the burrow, but there are no more tez left
in that burrow.) If the former (outstanding kits) is greater than the
latter (kits in circulation), the adjustment fee is increased and the
extra kits are burned. If some burrows are left unfilled, this restores
the balance.

The adjustment fee / bonus is capped at :math:`\pm 5` cNp per year, is
proportional to the imbalance in cNp and saturates when the imbalance
hits 20%.

This means that if the system were to end up being undercollateralized,
the drift would become lower and dilute the value of the kit, whereas if
the system were to end up being overcollateralized the drift would
become higher concentrating the value of the kit.

Liquidation
-----------

In situations where a burrow is overburrowed and, furthermore, beyond
the liquidation threshold, it can be marked for liquidation by anyone.
Liquidation is the process of selling some or all of its tez collateral
at auction for kit, which will be burned to reduce the burrow’s
excessive outstanding kit balance.

There is a reward for marking a burrow for liquidation, equal to 0.1 cNp
of the tez collateral plus the burrow creation deposit.

Note that we rely directly on the target and *not* any kit / tez price
we might observe on-chain. The reason is that, kits being off target
should *not* cause a hardening or loosening of burrowing rules.

Once a burrow is marked for liquidation, one can determine the amount of
tez that needs to be sold for kit at the current :math:`tz^{minting}_t`
price in order to return the burrow in a state where any outstanding
kits could have just been minted (including refilling the burrow
creation deposit, in case another liquidation is later needed). If there
would not be enough tez to refill the creation deposit, everything is
liquidated and the burrow is simply closed.

That portion of the tez collateral is sent to a queue for auction and
the burrow is assigned a corresponding lot number. As the queue receives
tez to sell for kit, it chops them up in increments of
:math:`tez\_batch`. We suggest :math:`tez\_batch = 10,000~\textrm{xtz}`.
Each lot is given a lot number which is held by the burrows which
contributed the tez to the lot.

Portions of a burrow’s tez collateral may be queued in multiple lots,
due either to splitting of large amounts across lots, or to successive
partial liquidations.

Liquidation auction
-------------------

If there are any lots of tez collateral waiting to be sold for kit,
Checker starts an open, ascending bid auction. There is a reserve price
set using :math:`k_t` which declines exponentially over time as long as
no bid as been placed. Once a bid is placed, the auction continues.
Every bid needs to improve over the previous bid by at least 0.33 cNp
and adds the longer of 20 blocks or 20 minutes, to the time before the
auction expires.

When liquidating, we liquidate 10% more than we are currently computing.
We call a liquidation “warranted” when the burrow would have been
targettable for liquidation had we used, retrospectively, the average
price obtained in the liquidation auction. Once the liquidation price is
known (after an auction) we look at whether that liquidation was
“warranted” — that is, it was proven to be necessary. If it was, we
destroy 10% of the kit proceeds of the auction. These 10% do not go
towards reducing the outstanding kit balance of the burrow, they are
just gone, for everyone. If it turned out that a liquidation was not
warranted, all
100% of the liquidation proceeds are credited to the burrow.

CFMM
----

There is a CFMM (Constant Function Market Maker) exchange facility
attached to the checker contract. It is much like a standard CFMM
contract (including the ability to mint and redeem tokens representing
a contribution of liquidity to the contract) except that its balance
in kit increases over time as kits are minted out of burrows to pay
for part of the burrowing fee, and tez are sometimes forfeited to it
(TODO: still true?). This balance is adjusted any time the checker
contract is called, looking back at the last time the contract was
called and calculating the fee incurred in between.

The contract’s implied ctez/kit price is used as part of the parameter
calculations.
