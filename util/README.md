
# mock_oracle

Example commands:

```
ligo compile-contract mock_oracle.mligo main > mock_oracle.tz
```

```
tezos-client originate contract mock_oracle transferring 0 from bob running mock_oracle.tz --init '(Pair "tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6" 1000000)'
```

```
ligo compile-contract mock_cfmm_oracle.mligo main > mock_cfmm_oracle.tz
```

```
tezos-client originate contract mock_cfmm_oracle transferring 0 from bob running mock_cfmm_oracle.tz --init '(Pair "tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6" (Pair 1000000 1000000))'
```
