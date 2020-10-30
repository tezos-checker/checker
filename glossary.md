# Glossary


## Liquidation lot

A batch of [liquidation slices](#liquidation-slice) currently being auctionned of.

## Liquidation slice

Some amount of tez, tied to a burrow, which is inserted in the liquidation queue to be auctionned of for [kit](#kit)

## Liquidation queue

A dequeue implemented as a balanced binary tree representing an ordered list of [liquidation slices](#liquidation-slice).
Slices at the front of the queue are periodically batched into a [liquidation lot](#liquidation-lot)


## Imbalance

The ratio of the number of [circulating kits](#circulating-kits) to the number
of [outstanding kits](#outstanding-kits).

## Imbalance adjustment

A compouding fee or reward applied to [burrows](#burrows) which implicitely
increases or decreases the number of [outstanding kits](#outstanding-kits)
over time to bring it closer to the number of [circulating kits](#circulating-kits)
so as to bring the [imbalance](#imbalance) closer to 1.

## Kit

A coin / token created and destroyed as part of the system.

## Circulating kits

The number of kits that exist. See also: [outstanding kits](#outstanding-kits).

## Outstanding kits

The number of kits that it would take to close all currently open burrows.
See also: [circulating kits](#circulating-kits).

