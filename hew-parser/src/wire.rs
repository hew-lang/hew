//! Stable wire member numbers shared by source and generated declarations.

use std::collections::HashSet;

/// A declaration cannot assign unique, unreserved wire numbers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WireNumberError {
    /// Two members explicitly use the same number.
    Duplicate(u32),
    /// A member explicitly uses a reserved number.
    Reserved(u32),
    /// No number remains at or above the requested starting number.
    Exhausted,
}

/// Preserve explicit numbers and number missing members in declaration order.
///
/// Automatic numbers start at `start`, skipping every explicit and reserved
/// number, including those declared later. Reserved numbers may repeat.
///
/// # Errors
/// Returns an error for duplicate explicit numbers, explicit use of a reserved
/// number, or exhaustion of the `u32` number space.
pub fn assign_wire_numbers(
    explicit: &[Option<u32>],
    reserved: &[u32],
    start: u32,
) -> Result<Vec<u32>, WireNumberError> {
    let reserved: HashSet<_> = reserved.iter().copied().collect();
    let mut occupied = reserved.clone();
    for number in explicit.iter().flatten().copied() {
        if reserved.contains(&number) {
            return Err(WireNumberError::Reserved(number));
        }
        if !occupied.insert(number) {
            return Err(WireNumberError::Duplicate(number));
        }
    }
    let mut next = Some(start);
    explicit
        .iter()
        .map(|number| {
            if let Some(number) = number {
                return Ok(*number);
            }
            let mut number = next.ok_or(WireNumberError::Exhausted)?;
            while occupied.contains(&number) {
                number = number.checked_add(1).ok_or(WireNumberError::Exhausted)?;
            }
            next = number.checked_add(1);
            Ok(number)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{assign_wire_numbers, WireNumberError};

    #[test]
    fn implicit_numbers_skip_later_explicit_and_reserved_numbers() {
        assert_eq!(
            assign_wire_numbers(&[None, Some(0), None, Some(4), None], &[2, 2], 0),
            Ok(vec![1, 0, 3, 4, 5])
        );
        assert_eq!(assign_wire_numbers(&[None, None], &[1], 1), Ok(vec![2, 3]));
    }

    #[test]
    fn explicit_conflicts_and_number_exhaustion_are_errors() {
        assert_eq!(
            assign_wire_numbers(&[Some(3), Some(3)], &[], 0),
            Err(WireNumberError::Duplicate(3))
        );
        assert_eq!(
            assign_wire_numbers(&[Some(3)], &[3], 0),
            Err(WireNumberError::Reserved(3))
        );
        assert_eq!(
            assign_wire_numbers(&[None], &[], u32::MAX),
            Ok(vec![u32::MAX])
        );
        assert_eq!(
            assign_wire_numbers(&[None, None], &[], u32::MAX),
            Err(WireNumberError::Exhausted)
        );
        assert_eq!(
            assign_wire_numbers(&[None], &[u32::MAX], u32::MAX),
            Err(WireNumberError::Exhausted)
        );
    }
}
