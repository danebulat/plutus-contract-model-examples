# Plutus Contract Model Examples

## Overview

Usage examples of the Plutus contract model framework for property testing smart contracts 
on Cardano.

The smart contracts and off-chain code examples **do not use state machines**. 

### Examples

#### Guess Game V1 

A simple guessing game contract and bare-bones contract model implementation. Refer to this example 
to understand how to hook up a simple smart contract and its off-chain code to the `ContractModel` 
typeclass.

The `"lock"` endpoint produces a script UTXO  containing a datum with a hashed string (representing 
the secret) and some locked lovelace. The `"guess"` endpoint submits a transaction with a redeemer 
containing a guess string. Funds in the script UTXO are unlocked if the guess hash matches the hash 
stored in the datum. 

The `test/Spec/GuessGameV1.hs` module attempts to provide concise but useful comments to explain 
each method (at a high level) that we need to implement in the `ContractModel` typeclass.

##### Source Files

- `src/GuessGame/Game.hs`<br>
  Contains the validator script and off-chain code for this smart contract.

- `test/Spec/GuessGameV1.hs`<br>
  Contract model implementation and functions to invoke property tests with QuickCheck.

#### Guess Game V2

A more complex smart contract built upon Guess Game V1 that utilises a "guess token" and allows a 
transaction to withdraw a variable amount of lovelace from the script UTXO, providing a correct 
guess is given in the transaction.

The role of the guess token is to limit who can attempt to unlock funds in the script UTXO. The 
wallet holding the guess token is only allowed to make a guess. The guess token is transferred to 
another wallet when a correct guess is sent and validation passes.

##### Source Files

- `src/GuessGameV2/OnChain.hs`<br>
  Smart contract validator script and guess token minting policy.

- `src/GuessGameV2/OffChain.hs`<br>
  Off-chain endpoints that build and submit transactions.

- `src/GuessGameV2/Emulator.hs`<br>
  Emulator trace to test the endpoints and validator script.

- `test/Spec/GuessGameV2.hs`<br>
  Contract model implementation and functions to invoke property tests with QuickCheck.

## Running the Tests 

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

It may take some time before the test suite finishes and outputs results.

## Running Tests with GHCi 

The modules that implement `ContractModel` instances also define functions to run 
property tests in the IO monad or part of a Tasty `TestTree`:

- `testsIO`<br>
  Runs property tests without outputting data for each test.

- `testsIO'`<br>
  Runs property tests with verbose output enabled, displaying the randomly 
  generated action sequence and results for each test.

- `tests`<br>
  Called in the main test suite module.

How to run property tests in GHCi:

```
cabal repl plutus-contract-model-examples:test:tests 

> :l test/Spec/GuessGameV1.hs

# Run tests in IO monad 
> testsIO

# Run verbose tests in IO monad 
> testsIO'
```

## Resources for Learning `ContractModel`

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

