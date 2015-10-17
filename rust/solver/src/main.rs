use std::env;
use std::cell::Cell;
use std::io::prelude::*;
use std::fs::File;

// usage: solver <seed> <output>

struct PseudoRNG {
    seed: Cell<u32>
}

impl PseudoRNG {
    fn new(seed: u32) -> PseudoRNG {
        PseudoRNG {
            seed: Cell::new(seed)
        }
    }

    fn generate(&self) -> u32 {
        let s = self.seed.get() as u64;
        let ss = ((s * 1103515245 + 12345) & 0xFFFFFFFF) as u32;
        self.seed.set(ss);
        return (self.seed.get() >> 16) & 0x7FFF;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        println!("ERROR");
        return;
    }

    let seed: u32 = args[1].parse().unwrap();
    let prng = PseudoRNG::new(seed);

    let mut timeout = prng.generate() % 196 + 5;
    let mut numblocks = prng.generate() % 1901 + 100;
    let mut height = prng.generate() % 81 + 20;
    let mut width = prng.generate() % 181 + 20;

    if seed == 0 {
        timeout = 5;
        numblocks = 100;
        height = 20;
        width = 20;
    }

    let mut f = File::create(&args[2]).unwrap();

    let init;
    if width & 1 == 0 {
        init = 1;
    }
    else {
        init = 0;
    }

    let mut hoffset = init;
    let mut voffset = 0;

    for i in 0 .. numblocks {
        if hoffset > width {
            hoffset = init;
            continue;
        }

        f.write(b"D");
        f.write(b"C");

        if hoffset > width / 2 {
            for r in 0 .. hoffset - width / 2 {
                f.write(b"L");
            }
        }
        else {
            for l in 0 .. width / 2 - hoffset {
                f.write(b"R");
            }
        }
        hoffset += 1;

        for d in 0 .. height - 2 {
            f.write(b"D");
        }
    }
}
