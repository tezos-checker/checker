# Security Concerns

This list can be endless for a contract of this size, of course, but let's
start keeping notes of security concerns that we must certainly look into in
the short term.

## LIGO vs. Michelson

We recently noticed that different token kinds in checker, though having
different types in LIGO, could have the same type and representation in
Michelson (see [issue](https://github.com/tzConnectBerlin/huxian/issues/34)).
Since this is not just about data that checker deals with internally, but
refers to data sent over the wire, it is a serious hazard. For example, checker
can issue a number of `kit` (e.g. via `%mintKit`) and send them to
`Tezos.sender`, which then `Tezos.sender` can use as `liquidity` tokens to gain
both `kit` and `tez` (e.g. via `%removeLiquidity`).

Hopefully this is easy to fix, by tagging each kind of token with a different
runtime content (see
[here](https://github.com/tzConnectBerlin/huxian/pull/41)).

However, there might be more issues like this within checker, where we have
assumed that some cases cannot occur, because they are ill-typed in LIGO. LIGO,
useful though it is, makes certain cases look impossible, even if they are not.
At the end of the day, what is executed is Michelson, and Michelson alone.

## Re-entrancy

We have not tested/speculated on checker's vulnerability to re-entrancy
attacks yet, but it looks like one of the most common weaknesses.

Just a couple of sources:
* [camlCase's statement on Dexter](https://camlcase.medium.com/statement-on-dexter-eaa7eecd2147)
* [Tom's brief description](https://tezos-dev.slack.com/archives/G01AAK05N86/p1614223324016000?thread_ts=1614220765.015800&cid=G01AAK05N86)
