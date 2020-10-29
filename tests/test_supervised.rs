use rusticsom::SOM;
use ndarray::{Array2, Array1};
use csv;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::prelude::*;
use stopwatch::Stopwatch;
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Default)]
struct TestSamples {
    pub pos: String,
    pub challengee: String,
    pub witness_gateway: String,
    pub signal: f64,
    pub snr: f64,
    pub fspl: f64,
    pub label: u32,
}

#[derive(Serialize, Deserialize, Debug, Default)]
struct TestSamplesTag {
    pub pos: String,
    pub challengee: String,
    pub witness_gateway: String,
    pub signal: f64,
    pub snr: f64,
    pub fspl: f64,
    pub label: u32,
    pub slabel: String,
}

fn normalize_sig(rssi: f64, snr: f64, fspl: f64) -> [f64; 3] {
    let rout = (rssi - (-134.0))/((0.0-(-134.0)));
    let sout = (snr - (-19.0))/((17.0-(-19.0)));
    let fout = (fspl - (-164.0))/((0.0-(-164.0)));
    [rout, sout, fout]
    //[rssi, snr, fspl]
}

#[derive(Serialize, Deserialize, Debug, Default)]
struct NodeInfo {
    pub column: usize,
    pub row: usize,
    pub wins: u32,
}

#[test]
fn test_supervised() -> Result<(), Box<dyn std::error::Error>> {

    let mut samples: Vec<TestSamplesTag> = Vec::new();

    let rdr = csv::ReaderBuilder::new()
        .has_headers(true)
        .from_path("./data/aggregate_samples.csv")?;

    let iter = rdr.into_deserialize();

    for result in iter {
        let record: TestSamples = result.unwrap();
        let rec_tag: TestSamplesTag = TestSamplesTag {
            pos: record.pos,
            challengee: record.challengee,
            witness_gateway: record.witness_gateway,
            signal: record.signal,
            snr: record.snr,
            fspl: record.fspl,
            label: record.label,
            slabel: match record.label {
                1 => "real".to_string(),
                0 => "fake".to_string(),
                _ => "undefined".to_string(),
            }
        };
        samples.push(rec_tag);
    }
    let testsamplen = samples.len();
    let mut fmtdata: Vec<[f64; 3]> = Vec::new();
    let mut fmtclass: Vec<String> = Vec::new();
    for i in 0..testsamplen {
        let data = samples.get(i).unwrap();
        fmtdata.push(normalize_sig(data.signal, data.snr, data.fspl));
        fmtclass.push(data.slabel.clone());
    }
    let mut classes: HashMap<String, f64> = HashMap::new();
    classes.insert("real".to_string(), 0.0);
    classes.insert("fake".to_string(), 0.0);
    //classes.insert("undefined".to_string(), 0.0);

    let mut map = SOM::create(10, 10, 3, true, Some(0.5), None, None, None, Some(classes));
    let newdat = Array2::from(fmtdata);
    let newdat2 = newdat.clone();
    let newlabel = Array1::from(fmtclass);
/*
    let sw = Stopwatch::start_new();
    map.train_random(newdat, 1600);
    println!("TrainUnsupervised: {:?}", sw.elapsed());

    let dist_map = map.distance_map();
    println!("{:?}", dist_map);
*/
    let mut file = File::create("outputs/output_unsupervised.json")?;
    file.write_all(map.to_json()?.as_bytes())?;

    let sw2 = Stopwatch::start_new();
    map.train_random_supervised(newdat2, newlabel, 1600);
    println!("TrainSupervised: {:?}", sw2.elapsed());
    //map.initialize_classes(newdat2, newlabel);
    file = File::create("outputs/output_supervised.json")?;
    file.write_all(map.to_json()?.as_bytes())?;
    /*
    let mut count = 0;
    for x in newdat2.genrows() {
        let y = x.to_owned();
        let _winner = map.winner(y, Some(newlabel2[count].clone()));
        count += 1;
    }
    */

    //let sw2 = Stopwatch::start_new();
    //map.train_random_supervised(newdat_labels, newlabel, 1600);
    //println!("TrainSupervised: {:?}", sw2.elapsed());

    //let mut file = File::create("output_supervised.json")?;
    //file.write_all(map.to_json()?.as_bytes())?;
    Ok(())
}
