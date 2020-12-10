# Small benchmarks

For now, this folder is a test bed to measure how much gas the operations consume.

* To spin up a test network via Docker, I followed: https://assets.tqtezos.com/docs/setup/2-sandbox/ . Don't forget to set up your
`tezos-client` to use the testnet.

## Commahds

Compile to Michelson:

```
$ make
```

Originate the contract:

```
tezos-client originate contract testcontract transferring 400 from bob running entrypoint.tz --init "$(cat storage.tz)" --dry-run
```

Call the contract:

```
tezos-client call testcontract from bob --arg "$(ligo compile-parameter entrypoint.mligo main 'Add (1, 2)')" --dry-run
```

## Some microbenchmarks

These are from a Delphi network. Keep in mind that it is using a `(int, int) big_map`, so the serialization costs
are low. In the actual code we'll insert composite objects with many keys.

A no-op call took `3185.240` gas.

```
$ tezos-client call testcontract from bob --arg "$(ligo compile-parameter entrypoint.mligo main 'Nop')" --dry-run
...
    Transaction:
      Amount: ꜩ0
      From: tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6
      To: KT1FeGFXYaU2SxmaGuivku5TvAUKh1tA1Won
      Parameter: (Right Unit)
      This transaction was successfully applied
      Updated storage: 2
      Storage size: 106158 bytes
      Consumed gas: 3185.240
```

After adding ~1500 elements to a bigmap, a write cost ~335 (`3520-3185`) gas:

```
$ tezos-client call testcontract from bob --arg "$(ligo compile-parameter entrypoint.mligo main 'Add (1500, 1500)')" --dry
...
    Transaction:
      Amount: ꜩ0
      From: tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6
      To: KT1FeGFXYaU2SxmaGuivku5TvAUKh1tA1Won
      Parameter: (Left (Left (Pair 1500 1500)))
      This transaction was successfully applied
      Updated storage: 2
      Updated big_maps:
        Set map(2)[1500] to 1500
      Storage size: 106158 bytes
      Consumed gas: 3520.688
```

After adding ~1500 elements, a lookup cost ~604 (`3789-3185`) gas:

```
$ tezos-client call testcontract from bob --arg "$(ligo compile-parameter entrypoint.mligo main 'Lookup 1500')" --dry-run
...
    Transaction:
      Amount: ꜩ0
      From: tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6
      To: KT1FeGFXYaU2SxmaGuivku5TvAUKh1tA1Won
      Parameter: (Left (Right 1500))
      This transaction was successfully applied
      Updated storage: 2
      Storage size: 106158 bytes
      Consumed gas: 3738.609
```
