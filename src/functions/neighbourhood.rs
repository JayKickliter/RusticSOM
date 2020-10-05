//! Neighborhood function implementations.

use ndarray::Array2;
use std::f64::consts::PI;

/// Default neighborhood function.
///
/// Returns a two-dimensional Gaussian distribution centered at `pos`.
pub fn gaussian(dims: (usize, usize), pos: (usize, usize), sigma: f64) -> Array2<f64> {
    let div = 2.0 * PI * sigma.powi(2);

    let shape_fn = |(i, j)| {
        let x = (-((i as f64 - (pos.0 as f64)).powi(2) / div)).exp();
        let y = (-((j as f64 - (pos.1 as f64)).powi(2) / div)).exp();
        x * y
    };

    Array2::from_shape_fn(dims, shape_fn)
}
