{- Sample tests for Assignment 2 -}
import Test.HUnit
import Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language', epsilonClosure)

-- Create a few sample automata

-- the language that only accepts empty string
empty :: Automaton
empty = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]

-- a basic finite language that only accepts one string ("abc")
finiteBasic :: Automaton
finiteBasic = Automaton [0,1,2,3] ['a','b','c'] [(0,'a',1),(1,'b',2),(2,'c',3)] 0 [3]

-- a complex finite language that only accepts three strings ("ac", "acd", "b")
finiteComplex :: Automaton
finiteComplex = Automaton [0,1,2,3,4] ['a','b']
    [(0,'a',1),(0,'b',2),(1,'a',3),(3,'a',4)] 0 [2,3,4]

-- a basic infinite language that accepts an even number of 'a's
infiniteBasic :: Automaton
infiniteBasic = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]

-- a complex infinite language: accepts a string with an even number of at least 2 'a's and
-- any number of 'b's
infiniteComplex :: Automaton
infiniteComplex = Automaton [0,1,2] ['a','b']
    [(0,'a',1),(1,'a',2),(0,'b',0),(1,'b',1),(2,'b',2),(1,'a',0)] 0 [2]

-- a language with numerous useless states that only accepts the string "a"
useless :: Automaton
useless = Automaton [0,1,2,3,4,5] ['a','b','c']
    [(0,'a',1),(0,'b',2),(1,'c',5),(0,'c',3),(3,'a',4)] 0 [1]

-- a language with epsilon transitions that only accepts the empty string
epsilonEmpty :: Automaton
epsilonEmpty = Automaton [0,1,2,3,4] ['a'] [(0,' ',1),(1,' ',2),(2,' ',3),(3,'a',4)] 0 [3]

-- a finite language with epsilon transitions that only accepts the string "a"
epsilonFinite :: Automaton
epsilonFinite = Automaton [0,1,2] ['a'] [(0,' ',1),(1,'a',2)] 0 [1]

-- an infinite language with epsilon transitions that accepts any number of consecutive 'b's
-- and ends with an 'a'
epsilonInfinite :: Automaton
epsilonInfinite = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,'a',1),(2,'b',0)] 0 [1]

-- a language with numerous epsilon closures that only accepts the strings "ab" and "b"
epsilonNumerous :: Automaton
epsilonNumerous = Automaton [0,1,2,3,4,5,6] ['a','b']
    [(0,'a',1),(1,' ',2),(2,' ',3),(3,'b',5),(0,' ',4),(4,'b',5),(5,' ',6)] 0 [6]


-- TESTS

tableToDeltaTests :: Test
tableToDeltaTests = TestList [

    -- one possible transition
    [2] ~=? tableToDelta [(1,'f',2)] 1 'f',

    -- no transition is taken
    [] ~=? tableToDelta [(1,'f',2)] 1 'b',

    -- two possible transitions, test that initial state must match transition
    [2, 3] ~=? tableToDelta [(0,'f',1),(1,'f',2),(1,'f',3)] 1 'f',

    -- two possible transitions, test that symbol must match transition
    [1, 2] ~=? tableToDelta [(1,'f',1),(1,'f',2),(1,'g',3)] 1 'f',

    -- result states must be in alphabetical order
    [1, 2] ~=? tableToDelta [(1,'f',2),(1,'f',1),(1,'g',3)] 1 'f'

    ]

extendTests :: Test
extendTests = TestList [

    -- one possible result state
    [2] ~=? extend (tableToDelta [(1,'f',2),(2,'f',2)]) 1 "ff",

    -- two possible result states
    [2, 4] ~=? extend (tableToDelta [(1,'f',2),(2,'f',2),(2,'g',3),(2,'f',4)]) 1 "ff",

    -- result states must appear in alphabetical order
    [2, 4] ~=? extend (tableToDelta [(1,'f',2),(2,'f',4),(2,'g',3),(2,'f',2)]) 1 "ff",

    -- result states must not contain duplicates
    [4] ~=? extend (tableToDelta [(1,'f',2),(2,'f',4),(1,'f',3),(3,'f',4)]) 1 "ff",

    -- test that characters in the string must be read in the correct order
    [4] ~=? extend (tableToDelta [(1,'f',2),(2,'f',3),(2,'g',4),(1,'g',2)]) 1 "fg",

    -- no possible result states
    [] ~=? extend (tableToDelta [(1,'g',2),(2,'f',2)]) 1 "ff",

    -- several possible result states through several transition paths
    [6, 9, 11] ~=? extend (tableToDelta [(1,'f',2),(1,'f',3),(3,'g',4),(4,'h',5),
    (5,'i',6),(2,'g',7),(7,'h',8),(8,'i',9),(7,'h',10),(10,'i',11)]) 1 "fghi"

    ]

allStringsTests :: Test
allStringsTests = TestList [

    -- strings possible with the empty string
    [""] ~=? allStrings "" !! 0,

    -- characters are the different in string
    ["aa", "ab", "ba", "bb"] ~=? allStrings "ab" !! 2,

    -- test that strings appear in alphabetical order
    [[""], ["x", "y", "z"], ["xx", "xy", "xz", "yx", "yy", "yz", "zx", "zy", "zz"],
    ["xxx", "xxy", "xxz", "xyx", "xyy", "xyz", "xzx", "xzy", "xzz", "yxx", "yxy", "yxz", "yyx",
    "yyy", "yyz", "yzx", "yzy", "yzz", "zxx", "zxy", "zxz", "zyx", "zyy", "zyz", "zzx", "zzy",
    "zzz"]] ~=? take 4 (allStrings "zyx"),

    -- test that first element of list is the empty string
    [""] ~=? allStrings "pqr" !! 0,

    -- test that list elements do not contain duplicate strings
    [[""], ["c"], ["cc"], ["ccc"], ["cccc"], ["ccccc"], ["cccccc"]] ~=? take 7 (allStrings "c")

    ]

possibleOutcomesTests :: Test
possibleOutcomesTests = TestList [

    -- accepting the empty string
    [("",[0])] ~=? (possibleOutcomes empty 0) !! 0,

    -- accepting strings that have one character
    [[("",[0])],[("a",[1]),("b",[]),("c",[])]] ~=? take 2 (possibleOutcomes finiteBasic 0),

    -- accepting strings that have two characters
    [("aa",[3]),("ab",[]),("ba",[]),("bb",[])] ~=? possibleOutcomes finiteComplex 0 !! 2,

    -- accepting strings that only contain one distinct character
    [[("",[0])],[("a",[1])],[("aa",[0])],[("aaa",[1])],[("aaaa",[0])]]
    ~=? take 5 (possibleOutcomes infiniteBasic 0),

    -- test where automaton accepts an infinite language
    [("aa",[0,2]),("ab",[1]),("ba",[1]),("bb",[0])]
    ~=? (possibleOutcomes infiniteComplex 0) !! 2,

    -- test the start state on the same automaton for different accept states
    [("aa",[1]), ("ab",[0,2]), ("ba",[0,2]), ("bb",[1])]
    ~=? (possibleOutcomes infiniteComplex 1) !! 2

    ]

acceptTests :: Test
acceptTests = TestList [

    -- accept empty string
    True ~=? accept empty "",

    -- does not accept anything other than empty string
    False ~=? accept empty "a",

    -- automaton that accepts a finite language accepts a string
    True ~=? accept finiteBasic "abc",

    -- automaton that accepts a finite language accepting more than one kind of string
    True ~=? accept finiteComplex "b",

    -- see above test
    True ~=? accept finiteComplex "aaa",

    -- automaton that accepts an infinite language does not accept a string
    True ~=? accept infiniteBasic "aa",

    -- automaton that accepts an infinite language accepts a string
    False ~=? accept infiniteBasic "aaa",

    -- automaton that accepts an infinite language accepts a longer string
    True ~=? accept infiniteComplex "aababbabaa",

    -- automaton that accepts an infinite language does not accept a longer string
    False ~=? accept infiniteComplex "bbbabb"

    ]

languageTests :: Test
languageTests = TestList [

    -- language of a basic infinite automaton
    ["","aa"] ~=? take 2 (language infiniteBasic),

    -- language of a more complex infinite automaton
    ["aa","aab","aba","baa","aaaa"] ~=? take 5 (language infiniteComplex),

    "baabb" ~=? language infiniteComplex !! 20

    ]

eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

removeUselessTests :: Test
removeUselessTests =
    let prunedNone = removeUseless finiteComplex
        prunedOne = removeUseless empty
        prunedOneInfinite = removeUseless infiniteComplex
        prunedNumerous = removeUseless useless
    in
    TestList [

        -- all states are useful in an automaton that accepts a finite language
        True ~=? eq prunedNone finiteComplex,

        -- all states are useful in an automaton that accepts an infinite language
        True ~=? eq prunedOneInfinite infiniteComplex,

        -- one state is useless
        True ~=? eq prunedOne (Automaton [0] ['a'] [] 0 [0]),

        -- numerous states are useless, remove all of them
        True ~=? eq prunedNumerous (Automaton [0,1] ['a','b','c'] [(0,'a',1)] 0 [1])

        ]

isFiniteLanguageTests :: Test
isFiniteLanguageTests = TestList [

    -- the empty language is finite
    True ~=? isFiniteLanguage empty,

    -- test finiteness on a basic finite language
    True ~=? isFiniteLanguage finiteBasic,

    -- test finiteness on a complex finite language
    True ~=? isFiniteLanguage finiteComplex,

    -- test finiteness on a basic infinite language
    False ~=? isFiniteLanguage infiniteBasic,

    -- test finiteness on a complex infinite language
    False ~=? isFiniteLanguage infiniteComplex,

    -- test finiteness on an automaton that has useless states
    True ~=? isFiniteLanguage useless

    ]

language'Tests :: Test
language'Tests = TestList [

    -- the empty language only contains the empty string
    [""] ~=? language' empty,

    -- get language of a basic finite automaton, (blocked in language function)
    ["abc"] ~=? language' finiteBasic,

    -- get language of a complex finite automaton
    ["b","aa","aaa"] ~=? language' finiteComplex,

    ["","aa"] ~=? take 2 (language' infiniteBasic),

    ["aa","aab","aba","baa","aaaa"] ~=? take 5 (language' infiniteComplex),

    -- language  of an automaton that contains useless states
    ["a"] ~=? language' useless

    ]

epsilonClosureTests :: Test
epsilonClosureTests = TestList [

    -- state is at the front of epsilon closure
    [0,1,2,3] ~=? epsilonClosure epsilonEmpty [0],

    -- state is at the end of epsilon closure
    [3] ~=? epsilonClosure epsilonEmpty [3],

    -- epsilon closure in a finite automaton
    [0,1] ~=? epsilonClosure epsilonFinite [0],

    -- epsilon closure is in an infinite automaton
    [0,2] ~=? epsilonClosure epsilonInfinite [0],

    [1] ~=? epsilonClosure epsilonInfinite [1],

    -- the epsilon closure of one state in an automaton with numerous closures
    [0,4] ~=? epsilonClosure epsilonNumerous [0],

    -- the epsilon closure of numerous states in an automaton with numerous closures
    [0,2,3,4,5,6] ~=? epsilonClosure epsilonNumerous [0,2,5]

    ]

main :: IO ()
main = do
    --Put each call to "runTestTT" on a separate line
    runTestTT tableToDeltaTests
    runTestTT extendTests
    runTestTT allStringsTests
    runTestTT possibleOutcomesTests
    runTestTT acceptTests
    runTestTT languageTests
    runTestTT removeUselessTests
    runTestTT isFiniteLanguageTests
    runTestTT language'Tests
    runTestTT epsilonClosureTests
    return ()