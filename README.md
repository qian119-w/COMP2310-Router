# COMP2310/6310 Assignment 2 2021

Repository for [COMP2310 Assignment 2 "Passing the Message"](https://cs.anu.edu.au/courses/comp2310/assessment/assignment2/)

## Building

### GNAT Studio

To build in GNAT Studio, build or run the `test_routers` program.

### Manually

To build manually, run
```
gprbuild [-jN] [-Xmode=debug|release]
```

Set `N` to the number of files you want to build in parallel.

The scenario variables are as follows
- `mode`:
    Controls the level of optimisation and runtime checks built into the program
    - `debug` (default): Build with minimal optimisation and stricter runtime checks, good for debugging and low compile time
    - `release`: Build with high optimisation and looser runtime checks, good for testing performance of a correct program

For example, to compile a release build on 4 cores you can run:
```
gprbuild -j4 -Xmode=release
```

## Running

### Configuration

Available network topologies are denoted by name together with the parameters which are required for a given topology. For example, you can test your routers in a 4-d hypercube by:

```
./build/debug/test_routers -t Hypercube -d 4
```

Use the help flag to print the full configuration parameters
```
$ ./build/debug/test_routers -h

accepted options:
   [-t {Topology            : String   }] -> CUBE_CONNECTED_CYCLES
      by Size            : Line, Ring, Star, Fully_Connected
      by Degree, Depths  : Tree
      by Dimension, Size : Mesh, Torus
      by Dimension       : Hypercube, Cube_Connected_Cycles,
                           Butterfly, Wrap_Around_Butterfly
   [-s {Size                : Positive }] ->  20
   [-g {Degree              : Positive }] ->   3
   [-p {Depths              : Positive }] ->   4
   [-d {Dimension           : Positive }] ->   3
   [-c {Print connections   : Boolean  }] -> TRUE
   [-i {Print distances     : Boolean  }] -> TRUE
   [-w {Routers settle time : Seconds  }] ->  0.10
   [-o {Comms timeout       : Seconds  }] ->  0.10
   [-m {Test mode           : String   }] -> ONE_TO_ALL
      Available modes: One_to_All, All_to_One
   [-x {Dropouts            : Natural  }] ->   0
   [-r {Repeats             : Positive }] -> 100
   [-h {Show help           : Flag     }] -> TRUE
```
