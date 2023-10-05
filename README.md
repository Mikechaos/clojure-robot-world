# Robot World
A Clojure project that implements a domain-specific language (DSL) for simulating a block world where robots can move blocks between stacks.

## Overview
The project provides a DSL for simulating operations on stacks of blocks. The operations allow for blocks to be moved onto, over, or piled onto other blocks. Each operation ensures that the rules of the block world are adhered to, ensuring the integrity of the simulation.

## Getting Started
### Prerequisites
- Java JDK 8+
- [Leiningen](https://leiningen.org/#install)

### Setting Up
Clone the repository:

```bash
git clone https://github.com/yourusername/robot-world.git
cd robot-world
```

Use Leiningen to run the project:

```bash
lein run
```

To run the unit tests
```bash
lein test
```

To start a REPL session:
```bash
lein repl
```

## Operations
`move-onto`: Move a block onto another block after clearing both blocks.  
`move-over`: Move a block over another block after clearing the source block.  
`pile-onto`: Pile blocks onto another block after clearing the destination block.  
`pile-over`: Pile blocks over another block.  

### Usage
To use the operations, you can invoke them as functions in the Clojure REPL or in your code. The operations expect the world state (a collection of stacks of blocks) as their first argument, followed by the blocks you want to operate on.

Example:

```clojure

(-> [[1] [2] [3] [4] [5] [6] [7]]
    (move-onto 2 3)
    (move-over 1 3)
    (move-onto 6 7)
    (pile-over 7 3))
```
The above code moves block 2 onto block 3, then moves block 1 over block 3, then moves block 6 onto block 7, and finally piles all blocks over block 3.

## Project Structure
The project is bui
`helpers.clj`: Contains helper functions used by the DSL to manipulate the block world.  
`dsl2.clj`: Contains the core DSL implementation, including the macro definitions for the operations.  
`core_test.clj`: Contains the unit tests for the project.

### Unit tests


## License
MIT License
