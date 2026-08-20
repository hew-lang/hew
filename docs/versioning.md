# Versioning

Hew is pre-1.0. The version is `0.MINOR.PATCH`, and the minor number carries
the weight a major number will carry after 1.0.

## What each number means

**Minor — `0.6.0`, `0.7.0`.** The language surface may change. Programs valid
under `0.6` may not compile under `0.7`, and the release notes say what to
change. A minor release is where the edition is settled.

**Patch — `0.6.1`, `0.6.2`.** The language surface does not change. A program
that compiles under `0.6.0` compiles under `0.6.1` and means the same thing.

**Release candidate — `0.6.0-rc1`.** A proposed `0.6.0`, published so it can be
used. It is not a preview of intent; it is the release, offered for
verification.

## The test that decides

For any change, ask: **does this change the set of programs the compiler
accepts, or what an accepted program means?**

If yes, it is minor-release work. It cannot ship in a patch, no matter how
small the diff or how obviously desirable the change.

If no, it is patch work.

Two cases that look alike and are not:

- A pattern the documentation says is supported crashes the compiler. Fixing
  it makes the implementation match the documented surface. **Patch.**
- A pattern the documentation says is refused is made to work. This expands
  the accepted set. **Minor**, even though it is also "fixing" something.

The second case is why an edition must settle its refusals before its final
release. Every documented refusal in `0.6.0` is a commitment to keep refusing
until `0.7.0`, or to have already decided it belongs in `0.6.0`.

## What a release candidate is for

An rc exists to be installed and used. Its purpose is to surface what internal
testing cannot: real programs, real platforms, real workflows.

That has consequences:

- **An rc with no published artifacts has not happened.** If nobody can install
  it, it has produced no evidence, whatever its tag says.
- **An rc is promoted, not rewritten.** Going from `0.6.0-rcN` to `0.6.0`
  should carry defect fixes and edition decisions already scoped. If the
  surface changes materially, the candidate was not a candidate — cut `rcN+1`
  and let it be used.
- **A quiet rc is not a passed rc.** Silence means unused, not sound.

## Promoting a candidate to final

`0.6.0-rcN` becomes `0.6.0` when all of the following hold:

1. The candidate was published and usable, and was exercised against real
   programs beyond this repository.
2. No known crash, leak, or silently wrong answer on a documented-supported
   pattern.
3. Every documented refusal is a deliberate edition decision, recorded with its
   rationale and its alternative — not an unfinished feature described as a
   limitation.
4. The corpus gates are green on every supported platform, with no expected
   failures standing in for known defects.
5. The release notes describe the surface a user gets, including what is
   refused and why.

If a fix taken during promotion changes the accepted program set, the
promotion stops and a new candidate is cut. That is the whole discipline: the
thing users install last is the thing that ships.

## Deciding between `0.6.1` and `0.7.0`

After `0.6.0`, a defect that does not change the accepted set is `0.6.1`.
Anything that changes the surface waits for `0.7.0`.

This is why the surface work is front-loaded. A gap left open at `0.6.0` cannot
be closed in `0.6.x` — it waits for the next minor, and every program written
against `0.6` in the meantime is written around the gap.
