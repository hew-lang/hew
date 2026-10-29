import Lean

namespace FrameCleanup

inductive Event where
  | quiet | effect | release
  deriving DecidableEq, Repr

structure Block where
  events : List Event
  successors : List Nat
  deriving Repr

/-- Once released, only quiet events may follow. -/
def scan : Bool → List Event → Bool
  | _, [] => true
  | false, .quiet :: es => scan false es
  | false, .effect :: es => scan false es
  | false, .release :: es => scan true es
  | true, .quiet :: es => scan true es
  | true, _ :: _ => false

def released (initial : Bool) (es : List Event) : Bool :=
  initial || es.contains .release

def marked (marks : List Bool) (i : Nat) : Bool :=
  marks[i]?.getD false

def checkBlock (marks : List Bool) (i : Nat) (b : Block) : Bool :=
  scan (marked marks i) b.events &&
    (if released (marked marks i) b.events then
      b.successors.all (marked marks) else true)

def check (graph : List Block) (marks : List Bool) : Bool :=
  (marks.length == graph.length) &&
    graph.all (fun b => b.successors.all (fun j => j < graph.length)) &&
    (graph.zipIdx).all (fun (b, i) => checkBlock marks i b)

/-- Small-step interpretation of a block's events. -/
inductive Steps : Bool → List Event → Bool → Prop where
  | done (s) : Steps s [] s
  | quiet {s es t} : Steps s es t → Steps s (.quiet :: es) t
  | effect {es t} : Steps false es t → Steps false (.effect :: es) t
  | release {es t} : Steps true es t → Steps false (.release :: es) t

theorem scan_steps : ∀ s es, scan s es = true → Steps s es (released s es) := by
  intro s es
  induction es generalizing s with
  | nil => cases s <;> simp [scan, released, Steps.done]
  | cons e es ih =>
    cases s <;> cases e <;> simp [scan] <;> intro h
    · exact Steps.quiet (by simpa [released] using ih false h)
    · exact Steps.effect (by simpa [released] using ih false h)
    · exact Steps.release (by simpa [released] using ih true h)
    · exact Steps.quiet (by simpa [released] using ih true h)

end FrameCleanup

namespace FrameCleanup

/-- A safe execution already in the released state contains only quiet events. -/
theorem steps_released_quiet {s : Bool} {es : List Event} {t : Bool} (h : Steps s es t)
    (hs : s = true) : ∀ e ∈ es, e = .quiet := by
  induction h with
  | done => simp
  | quiet h ih =>
    intro e he
    simp only [List.mem_cons] at he
    rcases he with rfl | he
    · rfl
    · exact ih hs e he
  | effect => contradiction
  | release => contradiction

/-- A successful scan cannot contain a non-quiet event after a release in the same block. -/
theorem scan_release_suffix (pre suffix : List Event)
    (h : scan false (pre ++ [.release] ++ suffix) = true) :
    ∀ e ∈ suffix, e = .quiet := by
  have aux : ∀ (p : List Event) (state : Bool),
      scan state (p ++ [.release] ++ suffix) = true →
      ∀ e ∈ suffix, e = .quiet := by
    intro p
    induction p with
    | nil =>
      intro state hs
      cases state with
      | false =>
        have steps := scan_steps true suffix (by simpa [scan] using hs)
        exact steps_released_quiet steps rfl
      | true => simp [scan] at hs
    | cons event rest ih =>
      intro state hs
      cases state <;> cases event <;> simp [scan] at hs
      · exact ih false (by simpa using hs)
      · exact ih false (by simpa using hs)
      · exact ih true (by simpa using hs)
      · exact ih true (by simpa using hs)
  exact aux pre false h

theorem checked_block {graph : List Block} {marks : List Bool} {i : Nat} {b : Block}
    (h : check graph marks = true) (mem : (b, i) ∈ graph.zipIdx) :
    checkBlock marks i b = true := by
  have hc := (Bool.and_eq_true_iff.mp h).2
  exact (List.all_eq_true.mp hc) (b, i) mem

theorem scan_of_checked {graph : List Block} {marks : List Bool} {i : Nat} {b : Block}
    (h : check graph marks = true) (mem : (b, i) ∈ graph.zipIdx) :
    scan (marked marks i) b.events = true := by
  have hb := checked_block h mem
  exact (Bool.and_eq_true_iff.mp hb).1

theorem successor_marked {graph : List Block} {marks : List Bool} {i j : Nat} {b : Block}
    (h : check graph marks = true) (mem : (b, i) ∈ graph.zipIdx)
    (r : released (marked marks i) b.events = true) (edge : j ∈ b.successors) :
    marked marks j = true := by
  have hb := (Bool.and_eq_true_iff.mp (checked_block h mem)).2
  simp only [r, Bool.true_eq, ite_true] at hb
  exact (List.all_eq_true.mp hb) j edge

theorem marked_events_quiet {graph : List Block} {marks : List Bool} {i : Nat} {b : Block}
    (h : check graph marks = true) (mem : (b, i) ∈ graph.zipIdx)
    (m : marked marks i = true) : Steps true b.events true := by
  have hs := scan_of_checked h mem
  rw [m] at hs
  simpa [released] using scan_steps true b.events hs

/-- A CFG path entering a certified post-release block; arbitrary length and cycles allowed. -/
inductive Path (graph : List Block) : Nat → List Block → Prop where
  | stop (i) : Path graph i []
  | last {i b} : (b, i) ∈ graph.zipIdx → Path graph i [b]
  | next {i j b bs} : (b, i) ∈ graph.zipIdx → j ∈ b.successors →
      Path graph j bs → Path graph i (b :: bs)

theorem post_release_path {graph : List Block} {marks : List Bool}
    (h : check graph marks = true) : ∀ {i bs}, Path graph i bs →
      marked marks i = true → ∀ b ∈ bs, Steps true b.events true := by
  intro i bs path
  induction path with
  | stop => simp
  | @last i b mem =>
    intro mark x hx
    simp only [List.mem_singleton] at hx
    subst x
    exact marked_events_quiet h mem mark
  | @next i j b bs mem edge path ih =>
    intro mark x hx
    have events := marked_events_quiet h mem mark
    have r : released (marked marks i) b.events = true := by simp [released, mark]
    have nextmark := successor_marked h mem r edge
    simp only [List.mem_cons] at hx
    rcases hx with rfl | hx
    · exact events
    · exact ih nextmark x hx

/-- A release in a checked block marks every successor, beginning the protected region. -/
theorem release_marks_successors {graph : List Block} {marks : List Bool} {i j : Nat} {b : Block}
    (h : check graph marks = true) (mem : (b, i) ∈ graph.zipIdx)
    (rel : Event.release ∈ b.events) (edge : j ∈ b.successors) : marked marks j = true := by
  apply successor_marked h mem _ edge
  simp [released, List.contains_iff_mem, rel]

/-- Every event in any subsequent finite CFG prefix is quiet after a release. -/
theorem release_following_quiet {graph : List Block} {marks : List Bool}
    {i j : Nat} {b : Block} {bs : List Block}
    (h : check graph marks = true) (mem : (b, i) ∈ graph.zipIdx)
    (rel : Event.release ∈ b.events) (edge : j ∈ b.successors)
    (path : Path graph j bs) :
    ∀ x ∈ bs, ∀ e ∈ x.events, e = .quiet := by
  have mark := release_marks_successors h mem rel edge
  have safe := post_release_path h path mark
  intro x hx
  exact steps_released_quiet (safe x hx) rfl

example : check [⟨[.effect, .release, .quiet], [1]⟩, ⟨[.quiet], [1]⟩] [false, true] = true := by decide
example : check [⟨[.release], [1]⟩, ⟨[.effect], [1]⟩] [false, true] = false := by decide
example : check [⟨[.release, .release], []⟩] [false] = false := by decide
example : check [⟨[.release], [1]⟩] [false, true] = false := by decide
example : check [⟨[.release], [1]⟩] [false] = false := by decide

end FrameCleanup
