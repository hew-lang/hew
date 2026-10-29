//! Execution certificates for the compiled timer/actor/cleanup pipeline.
//! The ledger observes identities and causal boundaries, never elapsed time.
pub const CASES: usize = 32;
pub const OWNERS: usize = 80;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(kani, derive(kani::Arbitrary))]
pub enum Kind {
    #[default]
    Lease,
    Timer,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(kani, derive(kani::Arbitrary))]
pub struct Row {
    pub present: bool,
    pub kind: Kind,
    pub case: u8,
    pub owner: u8,
    pub started: bool,
    pub closed: bool,
}

impl Row {
    fn start(&mut self, owner: u8, target: Self) -> Result<(), &'static str> {
        if !self.present
            || self.kind != Kind::Timer
            || self.closed
            || self.started
            || owner == 0
            || self.owner != owner
            || !target.present
            || target.kind != Kind::Lease
            || target.closed
            || target.case != self.case
            || !matches!(self.case % 8, 1 | 2)
        {
            return Err("handler duplicated, forbidden, or attached to the wrong incarnation");
        }
        self.started = true;
        Ok(())
    }
    fn close(&mut self, owner: u8) -> Result<(), &'static str> {
        if !self.present || self.closed || owner == 0 || self.owner != owner {
            return Err("duplicate or misdirected resource retirement");
        }
        self.closed = true;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(kani, derive(kani::Arbitrary))]
pub struct Case {
    pub quiescent: bool,
    pub leases: u8,
    pub timers: u8,
    pub faults: u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(kani, derive(kani::Arbitrary))]
pub struct Ledger {
    pub rows: [Row; OWNERS],
    pub cases: [Case; CASES],
    pub issued: u8,
    pub ended: bool,
}
impl Default for Ledger {
    fn default() -> Self {
        Self {
            rows: [Row::default(); OWNERS],
            cases: [Case::default(); CASES],
            issued: 0,
            ended: false,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Event {
    Issue {
        id: u8,
        kind: Kind,
        case: u8,
        owner: u8,
    },
    Start {
        id: u8,
        owner: u8,
    },
    Close {
        id: u8,
        owner: u8,
    },
    Fault {
        owner: u8,
        case: u8,
        kind: u8,
    },
    Quiescent {
        case: u8,
    },
    End,
}

impl Ledger {
    fn open_case(&self, case: u8) -> Result<Case, &'static str> {
        let case = self
            .cases
            .get(usize::from(case))
            .ok_or("unknown scenario")?;
        if case.quiescent {
            return Err("event after scenario quiescence");
        }
        Ok(*case)
    }
    fn row(&self, id: u8) -> Result<Row, &'static str> {
        let row = self
            .rows
            .get(usize::from(id.checked_sub(1).ok_or("zero identity")?))
            .filter(|row| row.present)
            .ok_or("unknown resource identity")?;
        Ok(*row)
    }
    pub fn apply(&mut self, event: Event) -> Result<(), &'static str> {
        if self.ended {
            return Err("event after final certificate");
        }
        match event {
            Event::Issue {
                id,
                kind,
                case,
                owner,
            } => {
                let state = self.open_case(case)?;
                if usize::from(id) > OWNERS || id == 0 || self.issued.checked_add(1) != Some(id) {
                    return Err("resource identity missing, duplicated or reordered");
                }
                let owner = match kind {
                    Kind::Lease => {
                        if owner != 0
                            || state.leases >= 2
                            || self
                                .rows
                                .iter()
                                .any(|row| row.present && row.case == case && !row.closed)
                        {
                            return Err(
                                "replacement incarnation preceded old ownership retirement",
                            );
                        }
                        id
                    }
                    Kind::Timer => {
                        let target = self.row(owner)?;
                        if state.timers != 0
                            || target.kind != Kind::Lease
                            || target.closed
                            || target.case != case
                        {
                            return Err("timer issued against the wrong or retired incarnation");
                        }
                        owner
                    }
                };
                self.rows[usize::from(id - 1)] = Row {
                    present: true,
                    kind,
                    case,
                    owner,
                    started: false,
                    closed: false,
                };
                match kind {
                    Kind::Lease => self.cases[usize::from(case)].leases += 1,
                    Kind::Timer => self.cases[usize::from(case)].timers += 1,
                }
                self.issued = id;
            }
            Event::Start { id, owner } => {
                let mut row = self.row(id)?;
                self.open_case(row.case)?;
                row.start(owner, self.row(owner)?)?;
                self.rows[usize::from(id - 1)] = row;
            }
            Event::Close { id, owner } => {
                let mut row = self.row(id)?;
                self.open_case(row.case)?;
                row.close(owner)?;
                self.rows[usize::from(id - 1)] = row;
            }
            Event::Fault { owner, case, kind } => {
                let state = self.open_case(case)?;
                let target = self.row(owner)?;
                if target.kind != Kind::Lease
                    || target.case != case
                    || target.closed
                    || state.faults != 0
                    || case % 8 < 4
                    || kind != u8::from(case % 8 != 4)
                {
                    return Err("fault missing its exact live owner or expected failure kind");
                }
                self.cases[usize::from(case)].faults = 1;
            }
            Event::Quiescent { case } => {
                let state = self.open_case(case)?;
                let faulted = case % 8 >= 4;
                if state.leases != 1 + u8::from(faulted)
                    || state.timers != 1
                    || state.faults != u8::from(faulted)
                    || self.rows.iter().any(|row| {
                        row.present
                            && row.case == case
                            && (!row.closed
                                || (row.kind == Kind::Timer
                                    && row.started != matches!(case % 8, 1 | 2)))
                    })
                {
                    return Err(
                        "quiescence left work, cleanup, restart or fault accounting unsettled",
                    );
                }
                self.cases[usize::from(case)].quiescent = true;
            }
            Event::End => {
                if usize::from(self.issued) != OWNERS
                    || self.cases.iter().any(|case| !case.quiescent)
                    || self.rows.iter().any(|row| !row.present || !row.closed)
                {
                    return Err("incomplete pipeline execution certificate");
                }
                self.ended = true;
            }
        }
        Ok(())
    }
}

#[cfg(kani)]
mod formal {
    use super::*;

    #[kani::proof]
    fn pipeline_identity_and_retirement_checker() {
        let mut row: Row = kani::any();
        let before = row;
        let target: Row = kani::any();
        let owner: u8 = kani::any();
        let start: bool = kani::any();
        let accepted = if start {
            row.start(owner, target)
        } else {
            row.close(owner)
        }
        .is_ok();
        if !accepted {
            assert_eq!(row, before, "pipeline: refusal preserves ownership");
        } else {
            assert!(
                before.present && !before.closed && owner != 0 && before.owner == owner,
                "pipeline: accepted operation must own the exact unretired resource"
            );
            assert_eq!(
                (row.kind, row.case, row.owner),
                (before.kind, before.case, before.owner)
            );
            if start {
                assert!(
                    !before.started && before.kind == Kind::Timer && row.started && !row.closed
                );
                assert!(
                    target.present
                        && target.kind == Kind::Lease
                        && !target.closed
                        && target.case == row.case
                );
                assert!(matches!(row.case % 8, 1 | 2));
            } else {
                assert!(
                    row.closed && row.started == before.started,
                    "pipeline: retirement neither repeats nor invents dispatch"
                );
            }
        }
        kani::cover!(accepted && start, "valid handler start");
        kani::cover!(accepted && !start, "valid unique retirement");
        kani::cover!(!accepted && before.closed, "closed identity refused");
        kani::cover!(
            !accepted && before.owner != owner,
            "different owner refused"
        );
    }

    #[kani::proof]
    #[kani::unwind(82)]
    fn pipeline_success_requires_global_retirement() {
        let mut ledger: Ledger = kani::any();
        let accepted = ledger.apply(Event::End).is_ok();
        let row: usize = kani::any();
        let case: usize = kani::any();
        kani::assume(row < OWNERS && case < CASES);
        if accepted {
            assert!(
                ledger.rows[row].present && ledger.rows[row].closed,
                "pipeline: final success cannot hide a live or missing resource identity"
            );
            assert!(
                ledger.cases[case].quiescent && usize::from(ledger.issued) == OWNERS,
                "pipeline: final success requires every scenario's quiescence barrier"
            );
        }
        kani::cover!(accepted, "complete execution accepted");
        kani::cover!(
            !accepted && !ledger.rows[row].closed,
            "outstanding cleanup refused"
        );
        kani::cover!(
            !accepted && !ledger.cases[case].quiescent,
            "incomplete scenario refused"
        );
    }
}
