# Simple Escrow Contract

## Introduction

The contract implements a mechanism of payments to wallets, but instead of doing
these payments directly, the lovelaces are blocked in a script, and then each wallet
can collect them.

## Contract Design

The contract has to store how many lovelaces correspond to each receiver. This
information is stored as a map from payment-pubkeyhashes to integers. So, the
main, and only, UTxO of the contract contains this map. The creation of this
main UTxO is perfomed by the `run` function that submits a transaction producing
the utxo containing the initial state where nobody has lovelaces to collect.

### Datums

Main UTxO:
- `EscrowState`: payments information

### Operations

- `addPayment`: given a payment-pubkeyhash `pkh` and an integer `m`, it submits
   a transaction paying to the script  `m` lovelaces and modifying the state specifying
   that now `pkh` has `m` additional lovelaces.
- `collect`: given a payment-pubkeyhash `pkh`, it submits a transaction spending
   the script-utxo and collecting the number of lovelaces specified in the state,
   corresponding to the pub key `pkh` signing the transaction.
   If the transaction can be submitted, the state is modified deleting the map
   association `pkh ↦ m` corresponding to the signer.

## Congestion

The two main operations produce congestion: both of them spend the script main
utxo, so only one user can interact with the contract per block.

## How to build

Here you can find information about how to build this project and run the on-chain
and off-chain tests.

#### Plutus dependencies

Install the development environment following the official [documentation](https://github.com/input-output-hk/plutus/tree/36e2c8bdbb6e70d25a31331e5cd23f26dc3162d5#how-to-build-the-projects-artifacts).
Once you have all installed, you will start the environment with `nix-shell`
from the plutus repository folder and change into the project folder.

#### Compilation

```
$> cabal build
```

#### Testing

```
$> cabal run test
```

#### More

You also can load the library or the tests with the repl with `cabal repl` or
`cabal repl test`.
