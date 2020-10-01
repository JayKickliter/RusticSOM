/// Core SOM definitions and functionality
///

use rand;
use ndarray;
use serde::{Serialize, Deserialize};
use rand_distr::{Normal, LogNormal};
use rand::random as random;
use rand::Rng;
use rand::distributions::{Distribution, Uniform};
use ndarray::{Array1, Array2, Array3, Axis, ArrayView1, ArrayView2};
use std::fmt;
use crate::functions::distance::euclid_dist;
use crate::functions::neighbourhood::gaussian;

#[derive(Serialize, Deserialize, Debug)]
pub struct DataLabel {
    label: String,
}
impl Default for DataLabel {
    fn default() -> DataLabel {
        DataLabel {
            label: "none".to_string(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SomData {
    x: usize,               // length of SOM
    y: usize,               // breadth of SOM
    z: usize,               // size of inputs
    learning_rate: f32,   // initial learning rate
    sigma: f32,           // spread of neighbourhood function, default = 1.0
    regulate_lrate: u32,    // Regulates the learning rate w.r.t the number of iterations
    pub map: Array3<f64>,       // the SOM itself
    pub tag_map: Option<Array2<DataLabel>>,
    pub activation_map: Array2<usize>,              // each cell represents how many times the corresponding cell in SOM was winner
}

pub struct SOM {
    pub data: SomData,
    decay_function: fn(f32, u32, u32) -> f64,          // the function used to decay learning_rate and sigma
    neighbourhood_function: fn((usize, usize), (usize, usize), f32) -> Array2<f64>,          // the function that determines the weights of the neighbours
}

pub enum DistType {
    Normal,
    Uniform,
    LogNormal,
}
// Method definitions of the SOM struct
impl SOM {
    // To create a Self-Organizing Map (SOM)
    pub fn create(
        length: usize,
        breadth: usize,
        inputs: usize,
        distribution: Option<DistType>,
        rand_range: Option<(f64,f64)>,
        learning_rate: Option<f32>,
        sigma: Option<f32>,
        decay_function: Option<fn(f32, u32, u32) -> f64>,
        neighbourhood_function: Option<fn((usize, usize), (usize, usize), f32) -> Array2<f64>>) -> SOM {
        // Map of "length" x "breadth" is created, with depth "inputs" (for input vectors accepted by this SOM)
        // randomize: boolean; whether the SOM must be initialized with random weights or not
        let mut the_map = Array3::<f64>::zeros((length, breadth, inputs));
        let act_map = Array2::<usize>::zeros((length, breadth));
        let mut _init_regulate_lrate = 0;

        match rand_range {
            Some(b) => {
                let mut rng = rand::thread_rng();
                match distribution {
                    Some(DistType::Uniform) => {
                        let uniform = Uniform::new(b.0, b.1);
                        for element in the_map.iter_mut() {
                            *element = uniform.sample(&mut rng);
                        }
                    }
                    Some(DistType::Normal) => {
                        let normal = Normal::new(b.0, b.1).unwrap();
                        for element in the_map.iter_mut() {
                            *element = normal.sample(&mut rng);
                        }
                    }
                    Some(DistType::LogNormal) => {
                        let ln = LogNormal::new(b.0, b.1).unwrap();
                        for element in the_map.iter_mut() {
                            *element = ln.sample(&mut rng);
                        }
                    }
                    None => {
                        for element in the_map.iter_mut() {
                            *element = random::<f64>();
                        }
                    }
                }
            }
            None => {
                for element in the_map.iter_mut() {
                    *element = random::<f64>();
                }
            }
        }

        let data = SomData {
            x: length,
            y: breadth,
            z: inputs,
            learning_rate: match learning_rate {
                None => 0.5,
                Some(value) => value,
            },
            sigma: match sigma {
                None => 1.0,
                Some(value) => value,
            },
            activation_map: act_map,
            map: the_map,
            tag_map: None,
            regulate_lrate: _init_regulate_lrate,
        };
        SOM {
            data,
            decay_function: match decay_function {
                None => default_decay_function,
                Some(foo) => foo,
            },
            neighbourhood_function: match neighbourhood_function {
                None => gaussian,
                Some(foo) => foo,
            },
        }
    }

    // To find and return the position of the winner neuron for a given input sample.
    pub fn winner(&mut self, elem: Array1<f64>) -> (usize, usize) {
        let mut temp: Array1<f64> = Array1::<f64>::zeros(self.data.z);
        let mut min: f64 = std::f64::MAX;
        let mut _temp_norm: f64 = 0.0;
        let mut ret: (usize, usize) = (0, 0);

        for i in 0..self.data.x {
            for j in 0..self.data.y {
                for k in 0..self.data.z {
                    println!("{:?}", self.data.map[[i, j, k]]);
                    println!("{:?}", elem[[k]]);
                    temp[k] = self.data.map[[i, j, k]] - elem[[k]];
                }

                _temp_norm = norm(temp.view());

                if _temp_norm < min {
                    min = _temp_norm;
                    ret = (i, j);
                }
            }
        }

        if let Some(elem) = self.data.activation_map.get_mut(ret) {
            *(elem) += 1;
        }

        ret
    }

    pub fn from_json(
        serialized: &str,
        decay_function: Option<fn(f32, u32, u32) -> f64>,
        neighbourhood_function: Option<fn((usize, usize), (usize, usize), f32) -> Array2<f64>>) -> serde_json::Result<SOM> {
        let data: SomData = serde_json::from_str(&serialized)?;

        Ok(SOM {
            data,
            decay_function: match decay_function {
                None => default_decay_function,
                Some(foo) => foo,
            },
            neighbourhood_function: match neighbourhood_function {
                None => gaussian,
                Some(foo) => foo,
            },
        })
    }

    pub fn to_json(&self) -> serde_json::Result<String> {
        serde_json::to_string_pretty(&self.data)
    }

    // Update the weights of the SOM
    fn update(&mut self, elem: Array1<f64>, winner: (usize, usize), iteration_index: u32) {
        let new_lr = (self.decay_function)(self.data.learning_rate, iteration_index, self.data.regulate_lrate);
        let new_sig = (self.decay_function)(self.data.sigma, iteration_index, self.data.regulate_lrate);

        let g = (self.neighbourhood_function)((self.data.x, self.data.y), winner, new_sig as f32) * new_lr;

        let mut _temp_norm: f64 = 0.0;

        for i in 0..self.data.x {
            for j in 0..self.data.y {
                for k in 0..self.data.z {
                    self.data.map[[i, j, k]] += (elem[[k]] - self.data.map[[i, j, k]]) * g[[i, j]];
                }

                _temp_norm = norm(self.data.map.index_axis(Axis(0), i).index_axis(Axis(0), j));
                for k in 0..self.data.z {
                    self.data.map[[i, j, k]] /= _temp_norm;
                }
            }
        }
    }

    // Trains the SOM by picking random data points as inputs from the dataset
    pub fn train_random(&mut self, data: Array2<f64>, iterations: u32) {
        let mut random_value: i32;
        let mut temp1: Array1<f64>;
        let mut temp2: Array1<f64>;
        self.update_regulate_lrate(iterations);
        for iteration in 0..iterations{
            temp1 = Array1::<f64>::zeros(ndarray::ArrayBase::dim(&data).1);
            temp2 = Array1::<f64>::zeros(ndarray::ArrayBase::dim(&data).1);
            random_value = rand::thread_rng().gen_range(0, ndarray::ArrayBase::dim(&data).0 as i32);
            for i in 0..ndarray::ArrayBase::dim(&data).1 {
                temp1[i] = data[[random_value as usize, i]];
                temp2[i] = data[[random_value as usize, i]];
            }
            let win = self.winner(temp1);
            self.update(temp2, win, iteration);
        }
    }

    // Trains the SOM by picking  data points in batches (sequentially) as inputs from the dataset
    pub fn train_batch(&mut self, data: Array2<f64>, iterations: u32) {
        let mut index: u32;
        let mut temp1: Array1<f64>;
        let mut temp2: Array1<f64>;
        self.update_regulate_lrate(ndarray::ArrayBase::dim(&data).0 as u32 * iterations);
        for iteration in 0..iterations{
            temp1 = Array1::<f64>::zeros(ndarray::ArrayBase::dim(&data).1);
            temp2 = Array1::<f64>::zeros(ndarray::ArrayBase::dim(&data).1);
            index = iteration % (ndarray::ArrayBase::dim(&data).0 - 1) as u32;
            for i in 0..ndarray::ArrayBase::dim(&data).1 {
                temp1[i] = data[[index as usize, i]];
                temp2[i] = data[[index as usize, i]];
            }
            let win = self.winner(temp1);
            self.update(temp2, win, iteration);
        }
    }

    // Update learning rate regulator (keep learning rate constant with increase in number of iterations)
    fn update_regulate_lrate(&mut self, iterations: u32){
        self.data.regulate_lrate = iterations / 2;
    }

    // Returns the activation map of the SOM, where each cell at (i, j) represents how many times the cell at (i, j) in the SOM was picked a winner neuron.
    pub fn activation_response(&self) -> ArrayView2<usize> {
        self.data.activation_map.view()
    }

    // Similar to winner(), but also returns distance of input sample from winner neuron.
    pub fn winner_dist(&mut self, elem: Array1<f64>) -> ((usize, usize), f64) {
        let mut tempelem = Array1::<f64>::zeros(elem.len());

        for i in 0..elem.len() {
            if let Some(temp) = tempelem.get_mut(i) {
                *(temp) = elem[i];
            }
        }

        let temp = self.winner(elem);

        (temp, euclid_dist(self.data.map.index_axis(Axis(0), temp.0).index_axis(Axis(0), temp.1), tempelem.view()))
    }

    // Returns size of SOM.
    pub fn get_size(&self) -> (usize, usize) {
        (self.data.x, self.data.y)
    }

    // Returns the distance map of each neuron / the normalised sum of a neuron to every other neuron in the map.
    pub fn distance_map(&self) -> Array2<f64> {
        let mut dist_map = Array2::<f64>::zeros((self.data.x, self.data.y));
        let mut temp_dist: f64;
        let mut max_dist: f64 = 0.0;
        for i in 0..self.data.x {
            for j in 0..self.data.y {
                temp_dist = 0.0;
                for k in 0..self.data.x{
                    for l in 0..self.data.y{
                        temp_dist += euclid_dist(self.data.map.index_axis(Axis(0), i).index_axis(Axis(0), j), self.data.map.index_axis(Axis(0), k).index_axis(Axis(0), l));
                    }
                }
                if temp_dist > max_dist {
                    max_dist = temp_dist;
                }
                dist_map[[i, j]] = temp_dist;
            }
        }
        for i in 0..self.data.x {
            for j in 0..self.data.y {
                dist_map[[i, j]] /= max_dist;
            }
        }
        return dist_map;
    }

    // Unit testing functions for setting individual cell weights
    #[cfg(test)]
    pub fn set_map_cell(&mut self, pos: (usize, usize, usize), val: f64) {
        if let Some(elem) = self.data.map.get_mut(pos) {
             *(elem) = val;
        }
    }

    // Unit testing functions for getting individual cell weights
    #[cfg(test)]
    pub fn get_map_cell(&self, pos: (usize, usize, usize)) -> f64 {
        if let Some(elem) = self.data.map.get(pos) {
             *(elem)
        }
        else {
            panic!("Invalid index!");
        }
    }
}

// To enable SOM objects to be printed with "print" and it's family of formatted string printing functions
impl fmt::Display for SOM {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (mut i, mut j) = (0, 0);

        for vector in self.data.map.lanes(Axis(2)) {
            println!("[{}, {}] : {}", i, j, vector);

            j += 1;
            if j == self.data.y {
                j = 0;
                i += 1;
            }
        }

        write!(f, "\nSOM Shape = ({}, {})\nExpected input vectors of length = {}\nSOM learning rate regulator = {}", self.data.x, self.data.y, self.data.z, self.data.regulate_lrate)
    }
}

// Returns the 2-norm of a vector represented as a 1D ArrayView
fn norm(a: ArrayView1<f64>) -> f64 {
    let mut ret: f64 = 0.0;

    for i in a.iter() {
        ret += i.powf(2.0);
    }

    ret.powf(0.5)
}

// The default decay function for LR and Sigma
fn default_decay_function(val: f32, curr_iter: u32, max_iter: u32) -> f64 {
    (val as f64) / ((1 + (curr_iter/max_iter)) as f64)
}

// Unit-testing module - only compiled when "cargo test" is run!
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_winner() {
        let mut map = SOM::create(2, 3, 5, Some(DistType::Uniform), Some((0.0, 1.0)), Some(0.1), None, None, None);

        for k in 0..5 {
            map.set_map_cell((1, 1, k), 1.5);
        }

        for k in 0..5 {
            assert_eq!(map.get_map_cell((1, 1, k)), 1.5);
        }

        assert_eq!(map.winner(Array1::from(vec![1.5; 5])), (1, 1));
        assert_eq!(map.winner(Array1::from(vec![0.5; 5])), (0, 0));
    }

    #[test]
    fn test_euclid() {
        let a = Array1::from(vec![1.0, 2.0, 3.0, 4.0]);
        let b = Array1::from(vec![4.0, 5.0, 6.0, 7.0]);

        assert_eq!(euclid_dist(a.view(), b.view()), 6.0);
    }
}
