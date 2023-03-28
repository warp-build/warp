// Define a simple struct to hold a point in 2D space
#[derive(Debug, PartialEq)]
struct Point {
    x: f64,
    y: f64,
    // more stuff
}

impl Point {
    fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }

    fn distance(&self, other: &Point) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }
}

// Define a simple function to calculate the sum of two numbers
fn add(a: i32, b: i32) -> i32 {
    a + b
}

// Define some unit tests for the Point struct and add function
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_distance() {
        let p1 = Point::new(0.0, 0.0);
        let p2 = Point::new(3.0, 4.0);
        assert_eq!(p1.distance(&p2), 5.0);
    }

    #[test]
    fn test_add() {
        assert_eq!(add(2, 2), 4);
        assert_eq!(add(10, -5), 5);
    }
}

