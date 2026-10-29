/-!
Executable specification for Hew's scheduled-work readiness gate. Kani
checks the production implementation against the rules emitted by this file.
-/
namespace ReadyGate

inductive Phase where
  | idle | queued | running | notified | complete
  deriving DecidableEq, Repr
structure State where
  phase : Phase
  frozen : Bool
  deriving DecidableEq, Repr
inductive Event where
  | notify | begin | freeze | unfreeze | pending | complete
  deriving DecidableEq, Repr
structure Outcome where
  state : State
  result : Bool
  deriving DecidableEq, Repr

def active (s : State) : Bool := s.phase == .running || s.phase == .notified
def demand (s : State) : Bool := s.phase == .queued || s.phase == .notified
def quiescent (s : State) : Bool := s.frozen && !active s

def step (s : State) (e : Event) : Option Outcome :=
  match e with
  | .notify =>
    let phase := match s.phase with
      | .idle => .queued | .running => .notified | other => other
    some ⟨⟨phase, s.frozen⟩, phase == .queued && s.phase == .idle && !s.frozen⟩
  | .begin =>
    if s.phase == .queued && !s.frozen then some ⟨⟨.running, false⟩, true⟩
    else some ⟨s, false⟩
  | .freeze => some ⟨{ s with frozen := true }, !active s⟩
  | .unfreeze => some ⟨{ s with frozen := false }, s.frozen && s.phase == .queued⟩
  | .pending => match s.phase with
    | .running => some ⟨⟨.idle, s.frozen⟩, false⟩
    | .notified => some ⟨⟨.queued, s.frozen⟩, !s.frozen⟩
    | _ => none
  | .complete => some ⟨⟨.complete, s.frozen⟩, false⟩

-- These finite proofs cover all states, not selected example histories.
theorem cannot_begin_active (s : State) (h : active s = true) :
    step s .begin = some ⟨s, false⟩ := by
  rcases s with ⟨p, f⟩
  cases p <;> cases f <;> simp_all [active, step]

theorem cannot_begin_frozen (p : Phase) :
    step ⟨p, true⟩ .begin = some ⟨⟨p, true⟩, false⟩ := by
  cases p <;> decide

theorem notification_during_poll (f : Bool) :
    step ⟨.running, f⟩ .notify = some ⟨⟨.notified, f⟩, false⟩ := by
  cases f <;> decide

theorem idle_notification_requests_poll (f : Bool) :
    step ⟨.idle, f⟩ .notify = some ⟨⟨.queued, f⟩, !f⟩ := by
  cases f <;> decide

theorem unfreeze_requests_retained_poll :
    step ⟨.queued, true⟩ .unfreeze = some ⟨⟨.queued, false⟩, true⟩ := by decide

theorem notified_poll_requeues (f : Bool) :
    step ⟨.notified, f⟩ .pending = some ⟨⟨.queued, f⟩, !f⟩ := by
  cases f <;> decide

theorem demand_retained (s : State) (e : Event) (o : Outcome)
    (notAcquired : e ≠ .begin ∨ o.result = false) (notComplete : e ≠ .complete)
    (before : demand s = true) (transition : step s e = some o) :
    demand o.state = true := by
  rcases s with ⟨p, f⟩
  cases p <;> cases f <;> cases e <;>
    simp_all [demand, step] <;> subst o <;> simp_all [demand]

theorem complete_absorbing (f : Bool) (e : Event) (o : Outcome)
    (transition : step ⟨.complete, f⟩ e = some o) : o.state.phase = .complete := by
  cases f <;> cases e <;> simp_all [step] <;> subst o <;> rfl

inductive Waiting : State → State → Prop where
  | same (s) : Waiting s s
  | advance {start s e o} : Waiting start s → (e ≠ .begin ∨ o.result = false) → e ≠ .complete →
      step s e = some o → Waiting start o.state

theorem demand_survives_arbitrary_wait {start finish : State}
    (waiting : Waiting start finish) (pending : demand start = true) :
    demand finish = true := by
  induction waiting with
  | same => exact pending
  | advance _ notBegin notComplete transition ih =>
    exact demand_retained _ _ _ notBegin notComplete ih transition

inductive Steps : State → State → Prop where
  | same (s) : Steps s s
  | advance {start s e o} : Steps start s → step s e = some o → Steps start o.state

theorem completion_is_permanent {f : Bool} {finish : State}
    (steps : Steps ⟨.complete, f⟩ finish) : finish.phase = .complete := by
  generalize startEq : (⟨.complete, f⟩ : State) = start at steps
  have initial : start.phase = .complete := by rw [← startEq]
  clear startEq
  induction steps with
  | same => exact initial
  | @advance s e o _ transition ih =>
    have phase := ih
    rcases s with ⟨p, frozen⟩
    simp only at phase
    subst p
    exact complete_absorbing _ _ _ transition

-- Ghost accounting records successful poll acquisition and relinquishment.
-- A finish is the caller's last access under its poll permit.
def ownersAfter (owners : Nat) (e : Event) (o : Outcome) : Nat :=
  match e with
  | .begin => if o.result then owners + 1 else owners
  | .pending | .complete => 0
  | _ => owners

def consistent (s : State) (owners : Nat) : Prop :=
  owners = if active s then 1 else 0

theorem step_preserves_owners (s : State) (e : Event) (o : Outcome) (owners : Nat)
    (before : consistent s owners) (transition : step s e = some o) :
    consistent o.state (ownersAfter owners e o) := by
  rcases s with ⟨p, f⟩
  cases p <;> cases f <;> cases e <;>
    simp_all [consistent, active, step, ownersAfter] <;> subst o <;>
    simp_all [consistent, active, ownersAfter]

inductive Execution : State → Nat → Prop where
  | initial : Execution ⟨.queued, false⟩ 0
  | idle : Execution ⟨.idle, false⟩ 0
  | advance {s owners e o} : Execution s owners → step s e = some o →
      Execution o.state (ownersAfter owners e o)

theorem arbitrary_execution_consistent {s : State} {owners : Nat}
    (execution : Execution s owners) : consistent s owners := by
  induction execution with
  | initial => rfl
  | idle => rfl
  | advance _ transition ih => exact step_preserves_owners _ _ _ _ ih transition

theorem arbitrary_execution_exclusive {s : State} {owners : Nat}
    (execution : Execution s owners) : owners ≤ 1 := by
  have h := arbitrary_execution_consistent execution
  unfold consistent at h
  split at h <;> omega

theorem frozen_quiescent_has_no_poller {s : State} {owners : Nat}
    (execution : Execution s owners) (quiet : quiescent s = true) : owners = 0 := by
  have h := arbitrary_execution_consistent execution
  unfold quiescent at quiet
  have inactive : active s = false := by
    cases f : s.frozen <;> cases a : active s <;> simp_all
  simpa [consistent, inactive] using h

def encode (s : State) : Nat :=
  let base := match s.phase with
    | .idle => 0 | .queued => 1 | .running => 2 | .notified => 3 | .complete => 4
  base + if s.frozen then 8 else 0

def decode : Nat → Option State
  | 0 => some ⟨.idle, false⟩ | 1 => some ⟨.queued, false⟩
  | 2 => some ⟨.running, false⟩ | 3 => some ⟨.notified, false⟩
  | 4 => some ⟨.complete, false⟩ | 8 => some ⟨.idle, true⟩
  | 9 => some ⟨.queued, true⟩ | 10 => some ⟨.running, true⟩
  | 11 => some ⟨.notified, true⟩ | 12 => some ⟨.complete, true⟩
  | _ => none

theorem decode_encode (s : State) : decode (encode s) = some s := by
  rcases s with ⟨p, f⟩
  cases p <;> cases f <;> decide

theorem encoding_fits_table (s : State) : encode s < 16 := by
  rcases s with ⟨p, f⟩
  cases p <;> cases f <;> decide

theorem freeze_encoding (s : State) :
    encode { s with frozen := true } = (encode s ||| 8) := by
  rcases s with ⟨p, f⟩
  cases p <;> cases f <;> decide

theorem unfreeze_encoding (s : State) :
    encode { s with frozen := false } = (encode s &&& 247) := by
  rcases s with ⟨p, f⟩
  cases p <;> cases f <;> decide

def events : List Event := [.notify, .begin, .freeze, .unfreeze, .pending, .complete]

-- One byte per oracle entry keeps exhaustive implementation checking compact.
def packed (o : Outcome) : Nat := encode o.state + (if o.result then 16 else 0) + 128

theorem packed_next (o : Outcome) : (packed o &&& 15) = encode o.state := by
  rcases o with ⟨⟨p, f⟩, result⟩
  cases p <;> cases f <;> cases result <;> decide

theorem packed_result (o : Outcome) : decide ((packed o &&& 16) ≠ 0) = o.result := by
  rcases o with ⟨⟨p, f⟩, result⟩
  cases p <;> cases f <;> cases result <;> decide

theorem packed_valid (o : Outcome) : (packed o &&& 128) = 128 ∧ packed o < 256 := by
  rcases o with ⟨⟨p, f⟩, result⟩
  cases p <;> cases f <;> cases result <;> decide

def rustTransition (raw : Nat) (e : Event) : String :=
  match decode raw >>= (fun s => step s e) with
  | none => "0"
  | some o => toString (packed o)

def rust : String :=
  "// Generated by proofs/ready_gate/ReadyGate.lean. Do not edit.\n" ++
  "#[rustfmt::skip]\npub(super) const TABLE: [[u8; 6]; 16] = [\n" ++
  String.join ((List.range 16).map fun raw =>
    s!"    [ // {raw}\n" ++ String.join (events.map fun e => "        " ++ rustTransition raw e ++ ",\n") ++ "    ],\n") ++
  "];\n#[rustfmt::skip]\npub(super) const QUIESCENT: [bool; 16] = [" ++
  String.intercalate ", " ((List.range 16).map fun raw =>
    toString ((decode raw).any quiescent)) ++ "];\n"

end ReadyGate

def main : IO Unit := IO.print ReadyGate.rust
