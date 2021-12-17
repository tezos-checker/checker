
A few notes about price calculation, given the recent generalization changes.
The following notes come from the discussion on [the corresponding
issue](https://github.com/tezos-checker/checker/issues/263#issuecomment-969045606).

Depending on the values of `tracking_type` (one of `index` or `token`) and
`collateral_type` (one of `tez` or `fa2`) in the configuration file, we have
three different possible setups (at the moment the combination (tracking=token
&& collateral=tez) is not allowed):

1. index-based, collateral = tez
2. index-based, collateral = fa2
3. token-based, collateral = fa2

For each case, the units of measure are as follows:

Index-based, collateral = wtez (TEZ)
---
```
(pegged to CHF)
collateral = TEZ
cfmm = { kit & ctez }
issued = kit

1. TEZ/CHF  (from the index)
2. ctez/kit (from Checker's CFMM)

3. TEZ/ctez (from ctez's CFMM)

(collateralization) Minting/liquidation needs TEZ/CHF : p1
(price adjustment)  Drift adjustment needs kit/CHF    : p1 / ( p2 * p3 )
```

Index-based, collateral = tok (FA2)
---
```
(pegged to CHF)
collateral = tok
cfmm = { kit & tok }
issued = kit

1. tok/CHF (from the index)
2. tok/kit (from Checker's CFMM)

(collateralization) Minting/liquidation needs tok/CHF : p1
(price adjustment)  Drift adjustment needs kit/CHF    : p1 / p2
```

Token-based, collateral = tok (FA2)
---
```
(replicating tzCHF)
collateral = tok
cfmm = { kit & tzCHF }
issued = kit

1. tok/tzCHF (from onchain CFMM, as an oracle)
2. tzCHF/kit (from Checker's CFMM)

(collateralization) Minting/liquidation needs tok/tzCHF : p1
(price adjustment)  Drift adjustment needs kit/tzCHF    : 1 / p2
```
