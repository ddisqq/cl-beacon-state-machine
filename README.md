# cl-beacon-state-machine

A pure Common Lisp implementation of the Proof-of-Stake beacon chain state machine. Zero external dependencies.

## Features

- Complete beacon state representation (validators, balances, slots, epochs)
- State transitions (process_slots, process_block)
- Validator registry operations (add, slash, initiate exit)
- Attestation processing with participation tracking
- Full epoch processing (justification, finalization, rewards, penalties, shuffling)
- Inlined SHA-256 implementation
- SBCL-native threading for concurrent state access

## Requirements

- SBCL (Steel Bank Common Lisp)
- ASDF (included with SBCL)

## Usage

```lisp
;; Load the system
(asdf:load-system :cl-beacon-state-machine)

;; Create a genesis state
(defvar *state* (beacon-sm:make-beacon-state :genesis-time (get-universal-time)))

;; Add validators
(let ((pubkey (make-array 48 :element-type '(unsigned-byte 8) :initial-element 1))
      (withdrawal-creds (make-array 32 :element-type '(unsigned-byte 8) :initial-element 2)))
  (let ((validator (beacon-sm:make-validator
                     :pubkey pubkey
                     :withdrawal-credentials withdrawal-creds
                     :effective-balance beacon-sm:+max-effective-balance+
                     :activation-epoch 0)))
    (beacon-sm:add-validator *state* validator beacon-sm:+max-effective-balance+)))

;; Process slots
(beacon-sm:process-slots *state* 32)  ; Advance to slot 32

;; Check current epoch
(beacon-sm:get-current-epoch *state*)  ; => 1

;; Get active validator count
(beacon-sm:get-active-validator-count *state*)
```

## Architecture

```
cl-beacon-state-machine/
  cl-beacon-state-machine.asd   ; System definition
  src/
    package.lisp    ; Package exports
    util.lisp       ; SHA-256, byte utilities
    state.lisp      ; BeaconState structure, constants
    validator.lisp  ; Validator management, shuffling
    attestation.lisp; Attestation processing
    epoch.lisp      ; Epoch transitions
    beacon.lisp     ; State transitions, block processing
  test/
    test-beacon.lisp; Test suite
```

## Key Types

- `beacon-state` - Complete chain state at a slot
- `validator` - Validator registry entry
- `checkpoint` - Justified/finalized checkpoint
- `attestation` - Validator attestation
- `attestation-data` - Core attestation fields

## Epoch Processing

Each epoch boundary triggers:

1. **Justification/Finalization** - Updates checkpoints based on supermajority
2. **Inactivity Updates** - Tracks validator participation
3. **Rewards/Penalties** - Distributes rewards, applies penalties
4. **Registry Updates** - Processes validator activations/exits
5. **Slashings** - Applies deferred slashing penalties
6. **Effective Balance Updates** - Recomputes effective balances
7. **Participation Rotation** - Rotates epoch participation flags

## Testing

```bash
sbcl --eval '(asdf:test-system :cl-beacon-state-machine)' --quit
```

## License

BSD-3-Clause. Copyright (c) 2024-2026 Parkian Company LLC.

## References

- [Ethereum Consensus Specs](https://github.com/ethereum/consensus-specs)
- [Phase 0 Beacon Chain Spec](https://github.com/ethereum/consensus-specs/blob/dev/specs/phase0/beacon-chain.md)
- [Altair Beacon Chain Spec](https://github.com/ethereum/consensus-specs/blob/dev/specs/altair/beacon-chain.md)
