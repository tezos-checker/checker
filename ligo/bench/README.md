# Small benchmarks

For now, this folder is a test bed to measure how much gas the operations consume.

* To spin up a test network via Docker, I followed: https://assets.tqtezos.com/docs/setup/2-sandbox/ . Don't forget to set up your
`tezos-client` to use the testnet.

## Commands

Compile to Michelson:

```
$ make
```

Originate the contract:

```
tezos-client originate contract testcontract transferring 400 from bob running entrypoint.tz \
  --init "$(cat storage.tz)" \
  --burn-cap 1
```

Call the contract:

```
arg='Lookup 1'
tezos-client call testcontract from bob \
  --arg "$(ligo compile-parameter entrypoint.mligo main "$arg")" \
  --burn-cap 1
```

## Some microbenchmarks

These are from a Delphi network.

A no-op call took `4075.528` gas.

```
arg='Nop'
...
      Storage size: 688 bytes
      Consumed gas: 4075.528```
```

After adding some elements to a bigmap (size of the bigmap does not seem to affect this),
a write cost ~544 (`4619-4075`) gas:

```
arg='Add (1, Branch { left=1; left_height=1; left_tez=1mutez; right_tez=1mutez; right_height=1; right=1; parent=1})'
...
      Storage size: 40754 bytes
      Paid storage size diff: 96 bytes
      Consumed gas: 4619.808
```

After adding ~1500 elements, a lookup cost ~750 (`4825-4075`) gas:

```
arg='Lookup 420'
...
      Updated storage: 0
      Storage size: 40946 bytes
      Consumed gas: 4828.313
```
