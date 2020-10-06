# Huxian
![a kit](https://i.imgur.com/3VmpA2q.jpg)

## Introduction

This technical document introduces the concept of a **robocoin** and describes
a hypothetical system named **Checker** to implement a robocoin on a Tezos
chain.

A robocoin is a coin that uses various feedback mechanisms to control its
supply. One might call it a cybercoin as well. There is no widely accepted
definition of the term, it is created for the purpose of this document.

## Concepts

The following sections define concepts which together work to form Checker.

### The clock

Any computation on a blockchain happens in discrete time. We mark the
timestamps at which the Checker system is updated as a series of increasing
timestamps, $t_i$. Ideally, these updates happen at every block and thus, under
the [Carthage protocol](https:/tezos.gitlab.io/protocols/006_carthage.html),
those timestamps are separated by about a minute each.

That said, the system is designed to be resilient to changes in the interblock
time and to missing the occasional updates.

### Target and quantity

In Checker, robocoins, denominated in “kit” are algorithmically balanced to
achieve a certain degree of steadiness with respect to a **quantity** and an
**index** forming a **target**.

The word "kit" is chosen because it's short, simple to pronounce, and means a
baby fox.

The **quantity**, $q_{t_i}$, expressed in $\mathrm{kit}^{-1}$, is a
time-dependent property of the system which can fluctuate upwards or downwards.

The **index**, expressed in $\mathrm{kit}$, is an external measure of value.
Examples of an index include:

* “the median hourly minimum wage across OCDE countries, expressed in kits”, and
* “the value of one CHF (Swiss Franc), expressed in kits”.

This index is provided through a combination of off-chain and on-chain oracles.

The **target** $p_{t_i}$ is the dimensionless product of the **index** and the
**quantity**. Examples of **target** include:

* “the median of $q_{t_i}$ hours of minimum wage across all OCDE countries as a number of kits”,
* “the minimal compensation, in kits, that an airline might owe a passenger, pursuant to the Vienna convention, should they lose $q_{t_i}$ kg of luggage as a number of kits”, and
* “how many kits for $q_{t_i}$ Swiss Francs”.

Intuitively, if the target $p_{t_i}$ is below $1$, then kits are, in a sense
“too expensive” and if the target is above $1$ then kits are, in a sense, “too
cheap”.

### The instantaneous drift

The **instantaneous drift** $d_{t}$, or just **drift**, is a dimensionless quantity which varies continuously over time. It is defined at all time $t$ implicitly by the parameters of the system.

The drift is implicitly continuous, piecewise-quadratic between two clock ticks
with continuous derivatives.

Essentially, our control mechanism provides $d'$ the derivative of $d$ at clock
ticks, and we interpolate quadratically between them.

We can set $d_0=d'_0=0$; the system will adjust automatically so the initial
values do not particularly matter (so long as they are not absurdly large).

$d'_{t_{i+1}}$ is computed as defined in the [algorithmic control section](#Algorithmic-control) and $d_{t_{i+1}}$ is then:

$$d_{t_{i+1}} = d_{t_i} + \frac{1}{2}(d'_{t_i} + d'_{t_{i+1}})(t_{i+1} - t_i)$$

The drift $d(t)$ is applied to the quantity such that $q'(t) = d(t) q(t)$, hence:

$$q_{t_{i+1}} = q_{t_i} \textrm{exp}\left(\left(d_{t_i} + \frac{1}{6}(2 d'_{t_i}+d'_{t_{i+1}})(t_{i+1}-t_{i})\right)(t_{i+1}-t_i)\right)$$

The term in the exponential is the product between

* the time span $(t_{i+1}-t_i)$, and
* a term $\left(d_{t_i} + \frac{1}{6}(2 d'_{t_i}+d'_{t_{i+1}})(t_{i+1}-t_{i})\right)$ which is the average of the quadratic function over the period.

Note that given the practical constants involved ($d$ is typically on the order
of $10^0$ to $10^1$ cNp / year), the exponential can be approximated by $exp(x) = 1+x$[^1].

### Oracles

A set of $n\_oracle$ oracle feeds $o^{1, t}, \ldots o^{n\_oracle, t}$ provides
the **tez** denominated value of the external index (e.g. 1 CHF). The median of
these oracles gives a feed which we label $tz_t$.

#### Filters

##### Protected index

The median of external oracles is itself filtered as follows:

1. Define the protected index

The protected indexe, $\widehat{tz}_t$ is defined as:

$$\widehat{tz}_{t_i} = \widehat{tz}_{t_{i-1}} \times \mathrm{clamp}\!\left(\frac{tz_{t_i}}{\widehat{tz}_{t_{i-1}} }, e^{-\epsilon (t_{i}-t_{i-1})}, e^{\epsilon (t_{i}-t_{i-1})}\right)$$

We suggest a value of $\epsilon = 0.05~\mathrm{cNp/min}$ -- that's about 72 cNp / day, so the filter can catch up to a 2x or 0.5x move in 24 hours, and a 3% move in an hour.

$\widehat{tz}_t$ is like the suspension of a car, it lags behind large moves, but is insensitive to spikes (real or fabricated).

In addition, we define the following prices [mjg suggest creating a subsection
for "Prices" and moving the indexes below down a level.  Or (because they're
quite short), perhaps the below is actually an itemised list]

##### Minting index

The feed $tz^{minting}_t = \max (tz_t, \widehat{tz}_t)$ is the maximum of $tz_t$ and $\widehat{tz}_t$.

##### Liquidation index

The feed $tz^{liquidation}_t = \min(tz_t, \widehat{tz}_t)$ is the miminum of $tz_t$ and $\widehat{tz_t}$.

#### Changing oracle feed

The set of feeds [mjg if symbol exists, put it here] is initially fixed. A
Tezos protocol upgrade is **strongly recommended** to give bakers the ability
to signal in each block support for the addition or removal of oracle feeds.
[mjg I think this may be what is intended:]
We **strongly recommend** that the current Tezos protocol be upgraded to allow
bakers to signal in each block support for adding or removing oracles.

#### Target

The Checker system includes a
[uniswap](https://uniswap.org/whitepaper.pdf)-like contract which gives an
indication of the price of kit in tez, $k_t$ with unit
$\mathrm{tez}~\mathrm{kit}^{-1}$. The target can be computed as

$$p_t = q_t tz_t / k_t$$

For example: suppose

$$\left\{\begin{array}{ccc}
tz_t & = & 0.36~\textrm{xtz}\\
k_t & = & 0.3~\textrm{xtz/kit}\\
q_t & = & 0.9~\mathrm{kit}^{-1}\end{array}\right.$$


Then $p_t = 1.08$, and since $p_t > 1$, we can stay that kit is too cheap.

We do not need to filter the target feed as it only affects the drift in a
bounded way that is, even if $tz_t$ experiences wild, short lived swing, it
will not have a major effect on the system.

## Algorithmic control

Consider the measure of imbalance

$$\log p_t = \log(q_t tz_t / k_t).$$

All logarithm values are expressed in cNp or centinepers (for small values, a
centineper is almost the same as a percentage point so you can safely read 2
cNp and 2% as roughly equivalent).

We define the rate of change of the drift $d_t$ as $d'_t$. At any clock tick
$t$, $d'_t$ is defined based on the imbalance:

$$
\left\{\begin{array}{ccc}
|\log p_{t_i}| < 0.5~\textrm{cNp} & \Rightarrow & d'_{t_{i+1}} = 0\\
0.5~\textrm{cNp} < |\log p_{t_i}| < 5~\textrm{cNp} & \Rightarrow & d'_{t_{i+1}} = \mathrm{sign}(\log p_{t_i}) 0.01~\textrm{cNp}/\textrm{day}^2\\
5~\textrm{cNp} < |\log p_{t_i}| & \Rightarrow & d'_{t_{i+1}} = \mathrm{sign}(\log p_{t_i}) 0.05~\textrm{cNp}/\textrm{day}^2\\
\end{array}
\right.
$$

It's easy to imagine models where $d'_{t_{i+1}}$ depends continuously on $\log p_{t_i}$ but
our intuition is that such models tend to be less robust than simple bang-bang
models such as the one above.

Two remarks:
1. The unit of $d_t$ is $\textrm{cNp}/\textrm{day}$ because it represents the growth or shrinking of $q_t$ per unit of time. Therefore, it is natural that the unit of $d'_t$ is in  $\textrm{cNp}/\textrm{day}^2$. To get a better intuition of those quantities suppose drift starts at 0 cNp / day and imbalance stays below -0.5 cNp but above -5 cNp for a month, the drift would grow to 0.3 cNp / day, and $q_t$ would increase by 4.65 cNp (about 4.76%). If imbalance stayed below 5 cNp for a month, the drift would go from 0 cNp / day to 1.5 cNp / day in a month, increasing $q_t$ by 23.25 cNp (about 26.18%).

2. When compared to MakerDAO this is essentially setting a rate of increase or decrease for a (potentially negative!) stability fee programmatically, based on prices, as opposed to votes.

## Burrows

Burrows are similar to CDPs in MakerDAO. Each burrow is an independent smart
contract, originated by the Checker contract.

**Important: the creator of the burrow can set the delegation address associated with it.**

Both tez and kit can be deposited in the burrow and, in the right conditions,
the owner can mint kits from the burrow or withdraw tez.

The actions that can be performed on a burrow are

(**todo** make this list more rigorous and sync with the OCaml mockups)
- changing permissions (see OCaml code in appendix for more details)
- mint kit ("burrowing")
- set the delegate
- burn kit (a terrible thing to do a baby fox)
- assign the burrow to some other party
- deposit tez
- whithdraw tez
- close the burrow
- mark the burrow for liquidation

### Burrowing and overburrowing

**Burrowing** is the act of minting kits out of a burrow. To avoid
overpopulation of kits, the burrowing it limited depending on the number of tez
in the burrow. Generally, kits can be minted so long as the tez in the burrow
exceeds $f$ times the number of minted kits multiplied by $q_t$ multiplied by
$tz^{minting}_t$. We propose $f = 2$.

Assume for instance $tz^{minting}_t = 0.36 \textrm{xtz}$ and $q_t = 1.015$. To
mint 10 kits, one would require $2 \times 10 \times 0.36 \times 1.015 = 7.308~\mathrm{xtz}$ in the burrow.

In situations where the number of kits outstanding exceeds $f q_t tz^{liquidation}_t$, the burrow can be marked for liquidation, as we'll see
later.

### Burrow fee

While a burrow has outstanding kits, it continuously incurs a burrow fee,
assessed in kits, on its outstanding kit balance. The fee is compounded and
implicitely added to the outstanding kit balance.

A 0.5 cNp fee per year is assessed and implicitely credited to a tez / kit
uniswap contract which is described below in this document. It's important that
this is done implicitely, i.e. whenever the uniswap contract it calls, it knows
exactly what its balance is.

Note: it might seem at first like the fee is "paid" for by the burrow creators
but, from an economic perspective, the view that it is paid for by the kit
holders is equally valid as the fee can be offset by an adjustment of the rift
adjustments.

### Imbalance adjustment

The imbalance adjustment takes the form of either a fee or a bonus. The exact
amount of the fee (or bonus) is set depending on the imbalance between the
number of kits in circulation and the outstanding number of kits that would
need to be burned to close all burrows.

In general those numbers should be equal but, imperfect liquidations could
cause the numbers to become different. If the former is greater than the
latter, the fee is increased and the extra kit are burned. If some burrows are
left unfilled, this restores the balance.

The fee / bonus is capped at $\pm 5$ cNp per year, is proportional to the
imbalance in cNp and saturates when the imbalance hits 20%.

This means that if the system were to end up being undercollateralized, the
drift would become lower and dilute the value of the kit, whereas if the system
were to end up being overcollateralized the drift would become higher
concentrating the value of the kit.

## Liquidation

In situations where the number of kits outstanding exceeds $f q_t tz^{liquidation}_t$,
the burrow can be marked for liquidation. [mjg underspecified whether whole
burrow gets liquidated or just an excess part of it]

There is a reward for doing so, equal to the greater of 5 kit, or 0.01 cNp of
the outstanding kit balance. The kit payed for the reward are minted out of the
burrow, even if it means further increasing the burrowing ratio. This is done
simply out of convenience.

Note that we rely directly on the target and *not* and any kit / tez price we
might observe on-chain. The reason is that, kits being off target should _not_
cause a hardening or loosening of burrowing rules.

Once a burrow is marked for liquidation, one can determine the amount of tez
that needs to be sold for kit at the current $tz^{minting}_t$ price in order to
return the burrow in a state where any outstanding kits could have just been
minted.

That portion of the tez collateral is sent to queue and the burrow is assigned
a set of lot numbers. As the queue receives tez to sell for kit, it chops them
up in increments of $tez\_batch$. We suggest $tez\_batch = 10,000~\textrm{xtz}$.
Each lot is given a lot number which is held by the burrows which contributed
the tez to the lot.

## Lots auction

If there are any lots waiting to be auctioned, Checker starts an open,
ascending bid auction. There is a reserve price set at $tz_t$ which declines
exponentially over time as long as no bid as been placed. Once a bid is placed,
the auction continues. Every bid needs to improve over the previous bid by at
least 0.33 cNp and adds the longer of 20 blocks or 20 minutes, to the time
before the auction expires.

The average price at which the collateral was sold tells us if the liquidation
was warranted or not. If it was (i.e. the burrow was undercollateralized) 20%
of any excess tez in the contract is forfeited to the uniswap contract (i.e. is
added to its liquidity).

## Uniswap

There is a uniswap contract attached to the checker contract. It is much like a
standard uniswap contract (including the ability to mint and redeem tokens
representing a contribution of liquidity to the contract) except that its
balance in kit increases over time as kits are minted out of burrows to pay for
part of the burrowing fee and tez are sometimes forfeited to it. This balance
is adjusted anytime the checker contract is called, looking back at the last
time the contract was called and calculating the fee incurred in between.

The contract's implied xtz/kit price is used as part of the oracle.

The right to bake for this contract is automatically auctioned off to the best
bidder, the proceeds of the auction only accrue to the pool with a delay of 1
cycle.

## Governance

Checker attempts to minimize governance. It may be necessary however to remove
a bad oracle feed or to tweak the parameters used in the system. We suggest a
protocol amendment where bakers signal in block their desire to remove or add
oracle feeds.


## Notes

- Checker is an FA2 contract with liquidity tokens and kit.
- Ideally it also supports utxo tickets for both of those things

----

## Actions

We use a monolithic contract with the following functionality

### Monolithic contract
- Touch contract to update drift
- Touch contract to update oracle values
- Transfer kit
- Burrow
  - Create
  - Issue kit upon request of a burrow
- Uniswap
  - Purchase kit
  - Purchase tez
  - Create liquidity share
  - Redeem liquidity share
  - Transfer liquidity share
(see OCaml code for a more rigorous interface definition)


### Burrow contract
- Set permissions
- Deposit / withdraw tez
- Set delegate
- Mint / burn kit
- Mark as being in liquidation
- Purchase collateral from a burrow ongoing liquidation
(see OCaml code for a more rigorous interface definition)


Lambdas are wrapped in bigmaps and loaded lazily to lower gas costs.


## Appendix : the auction queue.

The auction queue is based on an AVL tree. The nodes of the tree contain
collateral for liquidation, represented as a pair between a balance and a
burrow and the time at which they entered the queue. Every node also maintains
the total amount of liquidation  in its right child and in its left child.

When a new auction begins, the tree is cut into two trees, the first one
containing the earliest liquidation items summing up to exactly an amount of
exactly 10,000 tez, and the other one the  rest of it. This might entail
cutting a liquidation item in half  and doing a bit of tree surgery (the first
one is, in general, composed of two subtrees of the initial tree) but it
remains in O(log n).

The 10,000 are auctioned and the root of the tree of the items that were part
of this batch auction is stored in a map indexed by this auction's batch
number.

When contracts undergoing liquidation want to recuperate some of the funds that
they recouped through the auction, they indicate the path in the tree to their
bid and receive the corresponding amount.

This design has the property that amounts queued for liquidation can be
cancelled.


## Roadmap

1. Clean up and formalize this paper
2. Implement checker as a system in OCaml (mocking up the interaction with the chain)
3. Modify the OCaml implementation to ensure that all "heavy" storage is persisted to bigmaps
4. Flatten the Ocaml implementation to get rid of modules (replace with underscores for namespacing)
5. Convert the code to MLigo

Alternative: do not use Ligo, use meta-michelson in OCaml.

## Appendix : some code to formalize a bit the interface

```ocaml
(** A mock OCaml Implementation of Checker to serve as a specification *)

module Tezos : sig
  type address
  type entrypoint
  val address_of_string : string -> address option
  val get_entrypoint : address -> string -> address
  type nat
  type tez
  val tez_of_nat : nat -> tez
  val nat_of_int : int -> nat option
  type ratio = nat * nat

  (** Data available in a Tezos call, the amount sent and the sender. *)
  type call = {amount : tez ; sender : address }

  (** A ticket, a new proposed feature in Michelson. *)
  type 't ticket = {issuer : address ; amount : nat ; content : 't}

  (** A transaction coming out of some contract and paying tez to some address *)
  type payment = {destination : address ; amount : tez}
end = struct
  type address = string
  let address_of_string x = Some x
  type nat = int
  type tez = int
  let tez_of_nat x = x
  let nat_of_int x = if x < 0 then None else Some x
  let get_entrypoint address ep = address ^ "%" ^  ep
  type call = {amount : nat ; sender : address }
  type 't ticket = {issuer : address ; amount : nat ; content : 't}
  type payment = {destination : address ; amount : tez}
end


module Checker : sig
  type kit
  type kit_utxo
end = struct
  type kit = Tezos.nat
  type kit_utxo = unit Tezos.ticket
end

module rec Burrow : sig
  (** Type of a burrow *)
  type t

  (** A right can be an {b admin} right, which implies all rights,
      or a {b user} right which can include depositing tez,
      withdrawing tez, minting kits, burning kit, and setting
     the delegate. *)
  type rights = Admin | User of { deposit_tez : bool ; withdraw_tez : bool ; mint_kit : bool ; burn_kit : bool ; set_delegate : bool }

  (** A permission is a ticket containing a right. *)
  type permission = rights ticket

  (** Whether or not the contract accepts permissionless tez deposits. *)
  val allow_all_tez : t -> bool

  (**** Deposits and withdrawals, burns and mints ****)

  (** Simple deposit, accepting only tez with no parameters. Only possible if allow_all_tez is set to true. *)
  val default : t -> t

  (** Deposit tez and / or burn kits. If the burrow does not require a permission to deposit, the permission can be None. *)
  val deposit : t -> call:Tezos.call -> permission option -> Checker.kit_utxo option -> t

  (** Withdraw tez and/or mint_kits *)
  val withdraw : t -> Tezos.tez ->  Checker.kit -> Tezos.payment * Checker.kit_utxo * t

  (** Sets the delegate *)
  val set_delegate : t -> permission -> Tezos.address -> t

  (**** Setting permissions ****)

  (** Sets whether or not to accept all tez deposits without permissions. Requires admin. *)
  val set_allow_all_tez_deposits : t -> permission -> bool -> t

  (** Sets whether or not to accept all kit burns without permissions. Requires admin. *)
  val set_allow_all_kit_burns : t -> permission -> bool -> t

  (** Creates a new permission. Requires admin. *)
  val make_permission : t -> permission -> rights -> (permission * t)

  (** Requires admin. Increments a counter so that all previous permissions are now invalid and returns a new admin permission.
      This makes is easy to transfer an admin permission to another party. *)
  val invalidate_all_permissions : t -> permission -> (permission * t)


  (**** Liquidation related ****)


  (** - If the contract is not in liquidation and
         - It should not be liquidated : do nothing
         - If it should be liquidated : send an appropriate amount of tez to the liquidation queue and tip the caller
      - If the contract is in liquidation and
         - The amount in the liquidation queue is insufficient : send more to the liquidation queue
         - The amount in the liquidation queue is too high :
              - And that amount is 0: mark the contract as not being under liquidation anymore
              - And that amount is greater than 0: cancel the next upcoming liquidation for this contract

       To avoid hysteresis, it is assumed that tez sent to the liquidation will sell for, say 2/3 of
       the current price estimate.
   **)

  (** Anyone can call touch, this helps perform housekeeping operations on the burrow. *)
  val touch : t -> t

  (* A few thoughts. What does it mean to be liquidated? It means that a portion of the funds is going to be
     sent to a liquidation queue in the checker contract. That liquidation queue auctions out blocks of, say,
     10,000 tez for kits (or some price defined amount).

     When the tez is taken from the contract and placed in the queue, the amount and "bins" your collateral
     is placed in is recorded. When the sell happens, a claim can be made for the kits burned to be
     deducted from this contract debt at the pro-rata.

     If the contract has recovered and should not be in liquidation anymore, cancel the earliest bid
     in the auction queue and get some tez back. This can be called repeatedly until it's all
     cancelled.
  *)

 end = struct
  type t = unit
end
and Checker : sig
  (** Represents the state of a checker contract *)
  type t

  (** A ticket signed by Checker representing a balance in kit. *)
  type kit_utxo

  (** A ticket signed by Checker representing a balance in liquidity shares. *)
  type lqs_utxo

  type error = | Insufficient_collateral

  (***** Burrow creation and kint minting section *****)

  (** "touch" can be called by anyone, it performs housekeeping tasks on the contract state such as pulling in oracle values *)
  val touch : t -> t

  (** Creates and return a new burrow owned by owner, fund it if tez is passed to the call. *)
  val create_burrow : t -> call:Tezos.call -> owner:Tezos.address -> (Burrow.t * t)

  (** Mint kits. Must be called by a burrow. Mint a kit utxo and send it back to the calling burrow. *)
  val mint_kit : t -> call:Tezos.call -> amount:Tezos.nat -> (t, error)  result

  (***** uniswap section *****)

  (** Buys kit by sending tez. Kit, and possibly tez, are sent back. *)
  val buy_kit : t -> call:Tezos.call -> max_price:Tezos.ratio -> kit_utxo * Tezos.payment * t

  (** Buys tez by sending kit. Tez, and a possibly kit is sent back. *)
  val sell_kit : t -> call:Tezos.call -> kit_utxo -> min_price:Tezos.ratio -> kit_utxo * Tezos.payment * t

  (** Add liquidity to the pool by sending tez and/or kits. *)
  val add_liquidity: t -> call:Tezos.call -> kits:(Tezos.nat option) -> lqs_utxo * t

  (** Redeem lqs utxo. *)
  val redeem_liquidity : t -> call:Tezos.call -> lqs_utxo -> kit_utxo * Tezos.tez * t


end = struct
end

```

[^1]: MakerDAO's record high stability fee was 17.81 cNp / year (19.5% a year),
let's triple that and make it and assume a drift of 53.43 cNp (that's about
70.61% a year!), over one minute this would only equate  / year\%$ a year
(MakerDAO's stability fee's record high was $19.5\%$), over one minute, this
would equate about $53.43~/~(365.25 \times 24 \times 60) \mathrm{cNp} \simeq 1.016 \times 10^{-4} \mathrm{cNp}$.
The relative error in the approximation for $(e^x - (1+x))/ e^x$ is about
$5 \times 10^{-9}$. Even if the system does not update for a whole day, the
relative error would only be about $1.8 \times 10^{-5}$.

