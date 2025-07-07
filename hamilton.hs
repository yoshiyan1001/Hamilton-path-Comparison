-- represents CNF by list of list
--assingment represent dictonary key is variable and value is true or false
import Control.Exception
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (foldl', sortOn, zipWith)
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.CPUTime
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Random
import Text.Printf (printf)
import Text.Read (readMaybe)

type Clause = [Int]
type CNF = [Clause]
type AdjList = Map.Map Int [Int]

-- How to excute this code and run it
-- $ runghc Hamilton.hs -r <number of nodes> <number of edges> 
-- $ runghc Hamilton.hs -t <filename> 
-- for example:
-- $ runghc Hamilton.hs -r 5 5 
-- $ runghc Hamilton.hs -t graph_file.txt
-- We assume that the node names in the graph is from 1 to n and it must be positive integer.

create_literals :: CNF -> Clause
create_literals = concat

-- we flattens all literals acoss all clauses into one list.
pure_literal_branch :: CNF -> Clause ->  Map.Map Int Bool -> Maybe ( Map.Map Int Bool)
pure_Llteral_branch clauses [] assignment = Nothing
pure_literal_branch clauses (lit:set_literals) assignment =
    if not (find_unique_literal (-lit) (lit:set_literals))
        then dpll clauses (Map.insert (abs lit) (lit > 0) assignment)
        else pure_literal_branch clauses set_literals assignment
                 
                
find_unique_literal :: Int -> Clause -> Bool
find_unique_literal lit literals = lit `elem` literals

first_unassigned_var :: CNF ->  Map.Map Int Bool -> Maybe Int
first_unassigned_var clauses assignment = 
    let vars = [abs lit | clause <- clauses, lit <- clause]
    in case filter (\v -> Map.notMember v assignment) vars of
        (x:_) -> Just x
        [] -> Nothing

--we try to assign values (True or False)
choose_variable_branch :: CNF ->  Map.Map Int Bool -> Maybe ( Map.Map Int Bool)
choose_variable_branch [] assignment = Just assignment
choose_variable_branch clauses assignment =
    case first_unassigned_var clauses assignment of
        Nothing -> Nothing
        Just var -> 
            try_values [True, False]
            where
                try_values [] = Nothing
                try_values (val:vals) =
                    let new_assign = Map.insert var val assignment
                    in case dpll clauses new_assign of
                        Just result -> Just result
                        Nothing -> try_values vals


unit_clauses :: CNF -> Clause
unit_clauses [] = []
unit_clauses (clause: clauses) = 
    if length clause == 1
        then head clause : unit_clauses clauses
        else unit_clauses clauses

unit_propagation :: CNF ->  Map.Map Int Bool -> Maybe ( Map.Map Int Bool)
unit_propagation clauses assignment =
    case unit_clauses clauses of
        [] -> Nothing
        (lit:_) ->
            let newAssign = Map.insert (abs lit) (lit > 0) assignment
            in Just newAssign


is_emptyClauses :: CNF -> Bool
is_emptyClauses [] = False
is_emptyClauses (clause: clauses) =
    case clause of
        [] -> True
        (c:clause) -> is_emptyClauses clauses


simplify_clauses :: CNF ->  Map.Map Int Bool -> CNF
simplify_clauses [] _ = []
simplify_clauses (clause:clauses) assignment =
    let (new_clause, satisfied) = simplify_clause clause assignment 
    in if not satisfied
        then case new_clause of
            Just newClause -> newClause : simplify_clauses clauses assignment
            Nothing -> simplify_clauses clauses assignment  
        else simplify_clauses clauses assignment

simplify_clause :: [Int] ->  Map.Map Int Bool -> (Maybe [Int], Bool)
simplify_clause [] _ = (Just [], False)  
simplify_clause (literal : rest) assignment =
    case Map.lookup (abs literal) assignment of
        Just val ->
            if (literal > 0 && val) || (literal < 0 && not val)
                then (Nothing, True)  
                else simplify_clause rest assignment  
        Nothing ->
            let (maybeRest, satisfied) = simplify_clause rest assignment
            in case maybeRest of
                Nothing -> (Nothing, True) 
                Just simplified -> (Just (literal : simplified), False)

--DPLL algorithm
--this is the DPLL function to find the assingment from a formula (a graph)
--This solves to determine a graph has a hamilton path.

dpll :: CNF ->  Map.Map Int Bool -> Maybe (Map.Map Int Bool)
dpll clauses assignment =
    let simplified = simplify_clauses clauses assignment
    in if null simplified
        then Just assignment
    else if is_emptyClauses simplified
        then Nothing
    else case unit_propagation simplified assignment of
        Just newAssign -> dpll simplified newAssign
        Nothing ->
            let literals = create_literals simplified
            in case pure_literal_branch simplified literals assignment of
                Just assign' -> dpll simplified assign'
                Nothing -> choose_variable_branch simplified assignment

--read a file
read_file :: FilePath -> IO String
read_file file_name = readFile file_name

parse_graph :: String -> AdjList
parse_graph content =
    let ls = lines content
        edges = map (map read . words) ls
        number = zip [1..] edges
    in Map.fromList number

--avoiding self loop should fix it
generateUniqueEdges :: Int -> Int -> StdGen -> Set.Set (Int, Int) -> [(Int, Int)]
generateUniqueEdges _ 0 _ seen = Set.toList seen
generateUniqueEdges n m gen seen =
    let (u, gen1) = randomR (1, n) gen
        (v, gen2) = randomR (1, n) gen1
        edge = if u < v then (u, v) else (v, u)
    in if u == v || Set.member edge seen
        then generateUniqueEdges n m gen2 seen
        else generateUniqueEdges n (m - 1) gen2 (Set.insert edge seen)

-- Randomly generate m edges for a graph with n nodes
generateEdges :: Int -> Int -> IO [(Int, Int)]
generateEdges n m = do
    gen <- newStdGen
    let edgeList = generateUniqueEdges n m gen Set.empty 
    return edgeList


-- Build undirected adjacency list from edges
buildAdjList :: [(Int, Int)] -> Int -> AdjList
buildAdjList edges n =
    let withEdges = foldl' insertEdge Map.empty edges
        allNodes = [1..n] --make sure all nodes are in the graph
    in foldl' ensureNode withEdges allNodes
  where
    insertEdge adj (u, v) =
        Map.insertWith (++) u [v] $
        Map.insertWith (++) v [u] adj
    ensureNode adj u =
        Map.insertWith (++) u [] adj
-- Generate a random graph
randomGraph :: Int -> Int -> IO AdjList
randomGraph n m = do
    edges <- generateEdges n m
    return $ buildAdjList edges n

--- For varNum,  x_{v,p} gets a unique value.
varNum :: Int -> Int -> Int -> Int
varNum n v p = (v-1) * n + p

---  For positionConstraints, each position must be occupied by exactly one vertex
positionConstraints :: Int -> CNF
positionConstraints n =
    let posClauses = [[varNum n v p | v <- [1..n]] | p <- [1..n]]
        noDupes = [[-varNum n v1 p, -varNum n v2 p] | p <- [1..n], v1 <- [1..n], v2 <- [v1+1..n]]
    in posClauses ++ noDupes

--- For vertexConstraints, each vertex must appear exactly once in the path
vertexConstraints :: Int -> CNF
vertexConstraints n = 
    let vertexClauses = [[varNum n v p | p <- [1..n]] | v <- [1..n]]
        noDupes = [[-varNum n v p1, -varNum n v p2] | v <- [1..n], p1 <- [1..n], p2 <- [p1+1..n]]
    in vertexClauses ++ noDupes
--- For adjacencyConstraints, nonadjacent vertices cannot be consecutive in path
adjacencyConstraints :: Int -> AdjList -> CNF
adjacencyConstraints n adj =
    let isEdge u v = maybe False (v `elem`) (Map.lookup u adj)
        nonEdges = [(u, v)| u <- [1..n], v <- [1..n], u /= v, not (isEdge u v)]
        isBetweenClauses = [[-varNum n u p, -varNum n v (p+1)] | (u, v) <- nonEdges, p <- [1 .. (n-1)]]
    in isBetweenClauses

---generate CNF for hamilton 
generateHamiltonCNF :: AdjList -> CNF
generateHamiltonCNF adj =
    let n = Map.size adj
    in positionConstraints n ++ vertexConstraints n ++ adjacencyConstraints n adj

--decode the hamilton path 
decodeHamiltonPath :: Int ->  Map.Map Int Bool -> [Int]
decodeHamiltonPath n assignment =
    let trueVars = [(v, p) | v <- [1..n], p <- [1..n], let var = varNum n v p, Map.findWithDefault False var assignment]
    in map fst $ sortOn snd trueVars

--if the number of edges are more than upper bound, then return false.
isValidNodesEdges :: Int -> Int -> Bool
isValidNodesEdges nodes edges =
    let max = (nodes * (nodes -1)) `div` 2
    in edges <= max 

--construct random graph. If input length is not 2 or it is not valid number of edges and nodes, 
--print out error message.
construct_random_graph_with_params :: Int -> Int -> IO AdjList
construct_random_graph_with_params nodes edges = do
    if isValidNodesEdges nodes edges
        then randomGraph nodes edges
        else error "Invalid number of nodes or edges."


--finding Hamilton path by using DFS
findHamiltonPath :: AdjList -> Maybe [Int]
findHamiltonPath  adj =
    let nodes = Map.keys adj
        n = length nodes
    in dfsAllStart adj n

-- DFS with adjacent list
dfs :: AdjList -> Int -> [Int] -> Set.Set Int -> Maybe [Int]
dfs adj n path visited 
    | length path == n = Just (reverse path)
    | otherwise =
        case Map.lookup (head path) adj of
            Nothing -> Nothing
            Just neighbors -> tryNeighbors neighbors
    where
        tryNeighbors [] =Nothing
        tryNeighbors (x:xs)
            | x`Set.member` visited = tryNeighbors xs
            | otherwise =
                case dfs adj n (x:path) (Set.insert x visited) of
                    Just result -> Just result
                    Nothing -> tryNeighbors xs

--searching hamilton path from all nodes
dfsAllStart :: AdjList -> Int -> Maybe [Int]
dfsAllStart adj n = go (Map.keys adj)
    where
        go [] = Nothing
        go (v:vs) =
            case dfs adj n [v] (Set.singleton v) of
                Just path -> Just path
                Nothing -> go vs


compare_time :: Double -> Double -> IO ()
compare_time sat dfs
    | sat == dfs = putStrLn "SAT solver is as fast as DFS."
    | sat < dfs  = putStrLn "SAT solver is faster than DFS."
    | otherwise  = putStrLn "DFS is faster than SAT."

             
does_file_exist :: String -> IO Bool
does_file_exist file = doesFileExist file

cal_average_clause ::  (Integral a, Fractional b) => a -> a -> b -- divide two integers to make it Double
cal_average_clause total_literals num_clauses = fromIntegral total_literals / fromIntegral num_clauses

print_CNF_nums :: CNF -> IO()
print_CNF_nums cnf = do
    let num_clauses = length cnf
        num_vars = maximum $ map abs $ concat cnf
        total_literals = sum $ map length cnf
        avg_literals_per_clause = cal_average_clause total_literals num_clauses
    putStrLn $ "Number of variables: " ++ show num_vars
    putStrLn $ "Number of clauses: " ++ show num_clauses
    putStrLn $ "Average literals per clause: " ++ show avg_literals_per_clause
    putStrLn ""

result_measure :: AdjList -> Bool -> IO Double
result_measure adj isSAT = do
    start <- getCPUTime
    result <- if isSAT 
        then do
            let cnf = generateHamiltonCNF adj
            print_CNF_nums cnf
            let maybeAssign = dpll cnf Map.empty
            evaluate $ fmap (decodeHamiltonPath (Map.size adj)) maybeAssign
        else do
            let path =  findHamiltonPath adj
            evaluate path
    end <- getCPUTime
    case result of 
        Just path -> putStrLn $ "Hamiltonian Path found: " ++ show path
        Nothing -> putStrLn "No Hamiltonian Path exists."
    
    let diff = fromIntegral (end - start) / (10^12) :: Double
    printf "Computation time: %.6f seconds\n" diff
    return diff

main :: IO () --it does not generate 5 nodes graph, fromList [(2,[3,4,5]),(3,[2,4,5]),(4,[3,2]),(5,[3,2])] this test case does not work
main = do
    args <- getArgs
    case args of
        ["-r", nodesStr, edgesStr] -> do
            let nodes = read nodesStr :: Int
            let edges = read edgesStr :: Int
            adj <- construct_random_graph_with_params nodes edges
            putStr "The Random Graph is " 
            print adj
            putStrLn ""
            time_sat <- result_measure adj True
            putStrLn ""
            putStrLn "Next, look for Hamiltonian Path by DFS on the same graph."
            time_dfs <- result_measure adj False
            putStrLn ""
            compare_time time_sat time_dfs

        ["-t", file_name] -> do
            fileExists <- doesFileExist file_name
            if fileExists then do
                content <- read_file file_name
                let adj = parse_graph content
                time_sat <- result_measure adj True
                putStrLn ""
                putStrLn "Next, look for Hamiltonian Path by DFS on the same graph."
                time_dfs <- result_measure adj False
                putStrLn ""
                compare_time time_sat time_dfs
            else
                putStrLn "The file does not exist."

        _ -> putStrLn "Usage:\n  runghc Hamilton.hs -r <nodes> <edges>\n  runghc Hamilton.hs -t <filename>"
