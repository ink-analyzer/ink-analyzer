//! Test utilities for ink! analyzer.

use std::cmp;

/// Returns the offset of `pat` in `subject`.
///
/// offset is placed at the end of `pat` in `subject` by default,
/// unless `pat` is `Some` substring that starts with `<-`,
/// in which case the offset is placed at the beginning of the substring.
///
/// Additionally, `pat` is searched from the beginning of `subject` by default,
/// unless `pat` is `Some` substring that ends with `->`,
/// in which case `pat` is searched from the end of `subject`
/// (offsets are still calculated from the beginning of `subject` even in this case).
pub fn parse_offset_at(subject: &str, pat: Option<&str>) -> Option<usize> {
    let mut parsed_pat = pat;
    let mut position = Direction::End;
    let mut origin = Direction::Start;

    if let Some(substr) = parsed_pat {
        if substr.starts_with("<-") {
            // Strip the prefix and set offset position the `Start` variant if substring starts with "<-".
            parsed_pat = substr.strip_prefix("<-");
            position = Direction::Start;
        }

        if substr.ends_with("->") {
            // Strip the suffix and set the search origin the `End` variant if substring ends with "->".
            parsed_pat = parsed_pat.and_then(|substr| substr.strip_suffix("->"));
            origin = Direction::End;
        }
    }

    // Retrieve the offset using the structured utility.
    offset_at(subject, parsed_pat, position, origin)
}

/// An origin or placement direction.
enum Direction {
    Start,
    End,
}

/// Returns the offset of `pat` in `subject` using
/// `position` (i.e `Start` or `End`) to determine offset placement around the `pat` substring and
/// `origin` (i.e `Start` or `End`) to determine the search origin for the `pat` in `subject` (i.e beginning or end)
/// (offsets are still calculated from the beginning of `subject` regardless of the search origin/direction).
fn offset_at(
    subject: &str,
    pat: Option<&str>,
    position: Direction,
    origin: Direction,
) -> Option<usize> {
    match pat {
        Some(substr) => {
            // Origin determines how we search `subject` for the substring.
            let offset = match origin {
                Direction::Start => subject.find(substr),
                Direction::End => subject.rfind(substr),
            };
            // Position determines whether the offset is set at the beginning or end of the substring.
            match position {
                Direction::Start => offset,
                Direction::End => offset.map(|idx| cmp::min(idx + substr.len(), subject.len())),
            }
        }
        // No `pat` places offset at the beginning or the end of the subject depending on the desired position.
        None => match position {
            Direction::Start => Some(0),
            Direction::End => Some(subject.len()),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset_at_variants_works() {
        for (subject, pat, start_offset, end_offset, r_start_offset, r_end_offset) in [
            // subject - the subject being searched.
            // pat = substring used to find the cursor offset,
            // start_offset = the expected offset (calculated from the start of `subject`) when `subject` is searched
            //                from the beginning and the cursor is placed at the beginning of `pat`.
            // end_offset = the expected offset (calculated from the start of `subject`) when `subject` is searched
            //              from the beginning and the cursor is placed at the end of `pat`.
            // r_start_offset = the expected offset (calculated from the start of `subject`) when `subject` is searched
            //                  from the end and the cursor is placed at the beginning of `pat`.
            // r_end_offset = the expected offset (calculated from the start of `subject`) when `subject` is searched
            //                from the end and the cursor is placed at the end of `pat`.
            ("", None, Some(0), Some(0), Some(0), Some(0)),
            ("", Some(""), Some(0), Some(0), Some(0), Some(0)),
            ("", Some("a"), None, None, None, None),
            ("hello", None, Some(0), Some(5), Some(0), Some(5)),
            ("hello", Some(""), Some(0), Some(0), Some(5), Some(5)),
            ("hello", Some("e"), Some(1), Some(2), Some(1), Some(2)),
            ("hello", Some("l"), Some(2), Some(3), Some(3), Some(4)),
            ("hello", Some("lo"), Some(3), Some(5), Some(3), Some(5)),
            (
                "hello, world",
                Some("d"),
                Some(11),
                Some(12),
                Some(11),
                Some(12),
            ),
            (
                "hello, world",
                Some("o"),
                Some(4),
                Some(5),
                Some(8),
                Some(9),
            ),
            (
                "hello, world",
                Some(","),
                Some(5),
                Some(6),
                Some(5),
                Some(6),
            ),
            (
                "hello, world",
                Some("o,"),
                Some(4),
                Some(6),
                Some(4),
                Some(6),
            ),
            (
                "hello, world",
                Some(", "),
                Some(5),
                Some(7),
                Some(5),
                Some(7),
            ),
            // Ref: <https://doc.rust-lang.org/std/primitive.str.html#method.find>.
            // Ref: <https://doc.rust-lang.org/std/primitive.str.html#method.rfind>.
            (
                "Löwe 老虎 Léopard Gepardi",
                Some("ö"),
                Some(1),
                Some(3),
                Some(1),
                Some(3),
            ),
            (
                "Löwe 老虎 Léopard Gepardi",
                Some("老"),
                Some(6),
                Some(9),
                Some(6),
                Some(9),
            ),
            (
                "Löwe 老虎 Léopard Gepardi",
                Some("é"),
                Some(14),
                Some(16),
                Some(14),
                Some(16),
            ),
            (
                "Löwe 老虎 Léopard Gepardi",
                Some("éo"),
                Some(14),
                Some(17),
                Some(14),
                Some(17),
            ),
            (
                "Löwe 老虎 Léopard Gepardi",
                Some("老虎"),
                Some(6),
                Some(12),
                Some(6),
                Some(12),
            ),
            (
                "Löwe 老虎 Léopard Gepardi",
                Some("pard"),
                Some(17),
                Some(21),
                Some(24),
                Some(28),
            ),
        ] {
            // Start offset from beginning.
            assert_eq!(
                offset_at(subject, pat, Direction::Start, Direction::Start),
                start_offset
            );
            // End offset from beginning.
            assert_eq!(
                offset_at(subject, pat, Direction::End, Direction::Start),
                end_offset
            );

            // Start offset from end.
            assert_eq!(
                offset_at(subject, pat, Direction::Start, Direction::End),
                r_start_offset
            );
            // End offset from end.
            assert_eq!(
                offset_at(subject, pat, Direction::End, Direction::End),
                r_end_offset
            );

            if pat.is_some() {
                // Parse start offset from beginning, `<-` prefix only works if `pat` is Some.
                assert_eq!(
                    parse_offset_at(
                        subject,
                        pat.map(|substr| format!("<-{}", substr)).as_deref()
                    ),
                    start_offset
                );
            }
            // Parse end offset from beginning.
            assert_eq!(parse_offset_at(subject, pat), end_offset);

            if pat.is_some() {
                // Parse start offset from end, `<-` and `->` affixes only work if `pat` is Some.
                assert_eq!(
                    parse_offset_at(
                        subject,
                        pat.map(|substr| format!("<-{}->", substr)).as_deref()
                    ),
                    r_start_offset
                );

                // Parse end offset from end, `->` suffix only works if `pat` is Some.
                assert_eq!(
                    parse_offset_at(
                        subject,
                        pat.map(|substr| format!("{}->", substr)).as_deref()
                    ),
                    r_end_offset
                );
            }
        }
    }
}
