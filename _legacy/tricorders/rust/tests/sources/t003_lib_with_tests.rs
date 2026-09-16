pub fn add_two(a: i32) -> i32 {
    a + 2
}

#[cfg(test)]
mod tests {
	use super::*;

    #[test]
    fn add_two_and_two() {
        assert_eq!(4, add_two(2));
    }

    #[test]
    fn add_three_and_two() {
        assert_eq!(5, add_two(3));
    }

    #[test]
    fn one_hundred() {
		let res = super::add_two(100);
        assert_eq!(102, res);
    }
}

#[test]
fn no_mod_test_1() {
    assert_eq!(4, add_two(2));
}

#[test]
fn no_mod_test_2() {
    assert_eq!(5, add_two(3));
}

#[test]
fn add_4_and_two() {
    assert_eq!(6, add_two(4));
}
