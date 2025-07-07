# Comparison between SAT solver and DFS for Hamilton path 
## Goal
We compare the time between SAT solver(DPLL algorithm) and Depth First Search(DFS) to find the Hamilton path.
## DFS with Hamiltonian path
We simply apply for DFS on the graph.
## SAT Solver with Hamiltonian path

My project is to solve a hamiltonian path by using SAT solver. 

**Assumption, Defintion** 

Let $G = (V, E)$ be a indirected graph.
Defintion of a Hamiltonian path in G is a path that visits every vertex exactly once.

## Encoding the Problem
**Variables**
We define Boolean variables $x_{i, j} = True$ if vertex i is at position j in the path. 

### Constraints

**Constrant 1**
Every vertex appears in exactly one position in the path:

$\bigwedge^n_{i=1} \bigvee^n_{j=1} x_{i, j}$ 

This implies each vertex is in the path.
We ensure no vertex appears in two different positions:

$\bigwedge^n_{i=1} \bigwedge^n_{j=1} \bigwedge^n_{k=j+1} (\neg x_{i, j} \vee x_{i, k})$

**Constrant 2**
Each position in the path is occupied by exactly one vertex:
$\bigwedge^n_{j=1} \bigvee^n_{i=1} x_{i, j}$

This implies all positions in the path are filled.

Similarly, 
$\bigwedge^n_{j=1} \bigwedge^n_{i=1} \bigwedge^n_{k=j+1} (\neg x_{i, j} \vee x_{k, j})$

This implies no two vertices occupy the same position.
**Constrant 3**
If $(i, k) \notin E$ then $x_{i, j}$ and $x_{k, j+1}$ cannot both be true:

$\bigwedge_{(i, k) \notin E} \bigwedge^{n-1}_{j=1} (\neg x_{i, j} \vee \neg x_{k, j+1})$

This is mainly what I did in Python. Eeach constraint is contained in the functions, called Constaint1, Constaint2, Constaint3.

## How to run this code on the commandlines
If you want to produce a random graph:  
```
runghc Hamilton.hs -r <number of nodes> <number of edges>
```
For example,  
```
$ runghc Hamilton.hs -r 5 5
```
It produces the random graph that has 5 nodes and 5 edges.
If you want to apply it for a prepared graph on the file
```
$ runghc Hamilton.hs -t <filename>
```
For example:
```
$ runghc Hamilton.hs -t graph_file.txt
```
## Output
Once we use one of the commandlines. We will see for example,
```
The Random Graph is 
1: [5,3]
2: [5,4,3]
3: [2,1]
4: [2]
5: [2,1]

Number of variables: 25
Number of clauses: 150
Average literals per clause: 2.2

Hamiltonian Path found: [4,2,3,1,5]
Computation time: 0.010198 seconds

Next, look for Hamiltonian Path by DFS on the same graph.
Hamiltonian Path found: [3,1,5,2,4]
Computation time: 0.000040 seconds

DFS is faster than SAT.
```


