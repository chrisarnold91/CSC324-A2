{- Assignment 2 - Finite Automata (due November 11, noon)

Notes:
- You may import Data.List; you may not import any other modules

***Write the names and CDF accounts for each of your group members below.***
Christopher Arnold, g3arnold
-}

module Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language, 
            removeUseless, isFiniteLanguage, language', epsilonClosure) where

import Data.List

-- Basic data types
type State = Integer
type Symbol = Char
type Transition = (State, Symbol, State)

-- Automaton Data Type
-- Automaton states alphabet transitions initial final
data Automaton = Automaton [State] [Symbol] [Transition] State [State] deriving Show

-- Some helper functions for you to access the different automaton components
states :: Automaton -> [State]
states (Automaton s _ _ _ _) = s
alphabet :: Automaton -> [Symbol]
alphabet (Automaton _ a _ _ _) = a
transitions :: Automaton -> [Transition]
transitions (Automaton _ _ ts _ _) = ts
initial :: Automaton -> State
initial (Automaton _ _ _ i _) = i
final :: Automaton -> [State]
final (Automaton _ _ _ _ f) = f

-- More helper functions to access triple elements
getFirst :: (a, b, c) -> a
getFirst (first, _, _) = first
getSecond :: (a, b, c) -> b
getSecond (_, second, _) = second
getThird :: (a, b, c) -> c
getThird (_, _, third) = third


tableToDelta :: [Transition] -> State -> Symbol -> [State]
tableToDelta transitions = \state symbol -> sort (map (getThird) 
    (filter (\x -> (getFirst x) == state && (getSecond x) == symbol) transitions))

extend :: (State -> Symbol -> [State]) -> (State -> String -> [State])
extend f = \state symbols -> (sort . nub) (stringTransition f [state] symbols)

-- returns a list of states reachable by each symbol in a given string
stringTransition :: (State -> Char -> [State]) -> [State] -> [Char] -> [State]
stringTransition _ states "" = states
stringTransition f states (x:xs) = stringTransition f (symbolTransition f states x) xs

-- returns a list of states reachable by symbol
symbolTransition :: (State -> Symbol -> [State]) -> [State] -> Symbol -> [State]
symbolTransition f states symbol = concatMap (\x -> f x symbol) states

allStrings :: [Symbol] -> [[String]]
allStrings str = growList addAll [""] str

-- returns an infinite list where each element is created by adding each character
-- of the string to each of the previous element's strings
growList :: (t -> a -> a) -> a -> t -> [a]
growList f init str = init : growList f (f str init) str

-- returns a new list that appends each character in the string to each list element
addAll :: Ord a => [a] -> [[a]] -> [[a]]
addAll str lst = (sort . nub) (concatMap (\x -> addToEach [x] lst) str)

-- returns a new list with the character appended to the end of each list element
addToEach :: [a] -> [[a]] -> [[a]]
addToEach char lst = map (++ char) lst

possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes automaton q =
    possibleStates (transitions automaton) q (allStrings (alphabet automaton))

-- returns a list of list of tuples of all possible strings of the automaton's alphabet
-- with their respective final state(s)
possibleStates :: [Transition] -> State -> [[String]] -> [[(String, [State])]]
possibleStates transitions q strLsts =
    map (\strs -> zipWith (\a b -> (a, b)) strs (getStates transitions q strs)) strLsts

-- returns a list of lists that contain each state reachable from q by each
-- symbol in strings
getStates :: [Transition] -> State -> [String] -> [[State]]
getStates transitions q strings =
    map (\str -> ((extend . tableToDelta) transitions) q str) strings

-- Questions 5-6: acceptance
accept :: Automaton -> String -> Bool
accept automaton str = checkStates automaton str
    ((possibleOutcomes automaton (initial automaton)) !! length str)

-- returns True if outcomes contains a tuple with str and a final automaton state
checkStates :: Automaton -> String -> [(String, [State])] -> Bool
checkStates automaton str outcomes = any (\tuple -> str == fst tuple
    && (final automaton) `intersectBool` snd tuple) outcomes

-- returns True if lst1 and lst2 have at least one common element
intersectBool :: Eq a => [a] -> [a] -> Bool
intersectBool lst1 lst2 = any (`elem` lst1) lst2

language :: Automaton -> [String]
language automaton = languageHelper accept automaton
    ((concat . allStrings) (alphabet automaton))

-- returns a list of strings that the automaton accepts
languageHelper :: (Automaton -> String -> Bool) -> Automaton -> [String] -> [String]
languageHelper f automaton strs = filter (\str -> f automaton str) strs
    
removeUseless :: Automaton -> Automaton
removeUseless automaton =
    let t = transitions automaton
        f = final automaton
        useful = usefulTransitions f t
    in Automaton (finalBound f [] t getThird getFirst) (alphabet automaton)
        (usefulTransitions f t) (initial automaton) f

-- returns a list of transitions (removes the useless ones)
usefulTransitions :: [State] -> [Transition] -> [Transition]
usefulTransitions goals transitions =
    let useful = finalBound goals [] transitions getThird getFirst
    in filter (\transition -> getThird transition `elem` useful) transitions

-- returns a list of states that will eventually lead to the goal (lst)
finalBound :: [State] -> [State] -> [Transition] -> (Transition -> State)
    -> (Transition -> State) -> [State]
finalBound [] useful _ _ _ = sort useful
finalBound goal useful transitions fromState toState =
    let states = goalBound goal transitions fromState toState
    in finalBound (states \\ useful) (goal ++ useful) transitions fromState toState

-- returns a list of states that transition to the elements in lst
goalBound :: [State] -> [Transition] -> (Transition -> State)
    -> (Transition -> State) -> [State]
goalBound lst transitions fromState toState = ((\\ lst) . nub) (map (toState)
    (filter (\goal -> fromState goal `elem` lst) transitions))

isFiniteLanguage :: Automaton -> Bool
isFiniteLanguage automaton = 
    let pruned = removeUseless automaton
    in not (any (\str -> accept pruned str)
        (((allStrings . alphabet) pruned) !! (length . states) pruned))

language' :: Automaton -> [String]
language' automaton = if isFiniteLanguage automaton
    then languageHelper accept automaton
        ((concat . (take ((length . states) automaton))) (allStrings (alphabet automaton)))
    else language automaton

epsilonClosure :: Automaton -> [State] -> [State]
epsilonClosure automaton states =
    finalBound states [] (epsilonTransitions automaton) getFirst getThird

-- returns the automaton's epsilon transitions in a list
epsilonTransitions :: Automaton -> [Transition]
epsilonTransitions automaton =
    filter (\transition -> getSecond transition == ' ') (transitions automaton)