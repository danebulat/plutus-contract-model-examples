# Plutus Contract Model Examples

## Overview

This repository provides a starter project for learning the Plutus contract model framework.
The example contract is built with regular Plutus modules, and **does not use state machines**.

- `src/GuessGame/Game.hs`<br>
   A simple "guessing game" contract with two endpoints. The `"lock"` endpoint produces a script UTXO 
   containing a datum with a hashed string (representing the secret) and some locked lovelace. The 
   `"guess"` endpoint submits a transaction with a redeemer containing a string (representing the 
   guess). The funds in the script UTXO are unlocked if the guess hash matches the hash stored 
   in the datum. 

- `test/Spec.hs`<br>
  Defines the `ContractModel` instance for our `GameState` data structure. This module attempts to 
  provide concise but useful comments to explain each method (at a high level) that we need to implement 
  in the `ContractModel` typeclass.

## Running the tests 

Clone the `plutus-apps` repository and checkout tag `v1.0.0`.
The same tag is referenced in this project's `cabal.project` file.

```
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout v1.0.0 
```

Enter a `nix-shell` environment. Make sure to follow the setup instructions provided in
`plutus-apps` to correctly set up Nix and the IOHK hydra binaries.

```
nix-shell
```

Within `nix-shell`, clone this repository somewhere on your filesystem and build the project.


```
# Step outside plutus-apps directory
cd ..

# Clone this repository and enter its root directory
git clone https://github.com/danebulat/plutus-contract-model-examples 
cd plutus-contract-model-examples 

# Build the library and test suite 
cabal build 
```

Now we can run the test suite directly with `cabal`:

```
cabal run plutus-contract-model-examples:test:tests
```

## Running tests with GHCi 

Still in the Nix shell environment:

```
cabal repl plutus-contract-model-examples:test:tests 

> import Test.QuickCheck 
> quickCheck prop_Game 

```

## Resources for learning `ContractModel`

- [Plutus Pioneer Program Lecture 8](https://github.com/input-output-hk/plutus-pioneer-program)<br>
  During lecture 8, a `ContractModel` is defined for a token sale contract. Lars does a fantastic job 
  of introducing the QuickCheck library and the Plutus contract model. The token sale contract is
  built using state machines, which adds a bit more complexity to the example.

- [Plutus SDK Documentation - Tutorial - Contract Testing](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-testing.html)<br>
  The contract demonstrated in this tutorial is very similar to the `Game.hs` code in this repository. 
  However, it again uses state machines. The explanations and thought process behind writing 
  contract models demonstrated in this tutorial makes it well worth a read.

- [Plutus Use Cases Package (`plutus-use-cases`)](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-use-cases)<br>
  The `Game.hs` module in this repository is based on the same in this package. Other contract examples
  and testing approaches are also demonstrated.

