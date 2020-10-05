//! Node distance calculation functions.

use ndarray::ArrayView1;

/// Returns the Euclidean distance between `a` and `b`.
pub fn euclid_dist(a: ArrayView1<f64>, b: ArrayView1<f64>) -> f64 {
    debug_assert_eq!(
        a.len(),
        b.len(),
        "Euclidean distance requires equally length inputs"
    );

    a.iter()
        .zip(b.iter())
        .map(|(a, b)| (a - b).powi(2))
        .sum::<f64>()
        .sqrt()
}
