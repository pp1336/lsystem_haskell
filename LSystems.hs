module LSystems where

import Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (a , b , c) = a

-- |Returns the base string for the given system.
base :: System -> String
base (a , b , c) = b

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (a , b , c) = c


-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar ch ((c,r) : ru) 
  | ch == c = r
  | otherwise = lookupChar ch ru

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne ru st = concat (expandOne' ru st)
  where
  expandOne' :: Rules -> String -> [String]
  expandOne' ru' [] = []
  expandOne' ru' (s' : st') = lookupChar s' ru' : expandOne' ru' st'

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand ru st n = expand' ru st 0
  where
  expand' :: Rules -> String -> Int -> String
  expand' ru' st' n'
    | n' == n = st'
    | otherwise = expand' ru' (expandOne ru' st') (n'+1)

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
--  d = degree in radians
move :: Char -> TurtleState -> Float -> TurtleState
move ch ((x,y),a) fa
   | ch == 'F' = (((x + (cos d)),(y + (sin d))),a)
   | ch == 'L' = ((x,y),(a + fa))
   | ch == 'R' = ((x,y),(a - fa))
   | otherwise = ((x,y),a)
      where
      d = pi*a/180


-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
-- [TurtleState] is a stack to remeber TurtleState at each '['
trace :: String -> Float -> Colour -> [ColouredLine]
trace [] a col = [((0,0),(0,0),col)]
trace st a col
  = trace' st ((0,0),90) [((0,0),90)]
   where
    trace' :: String -> TurtleState -> [TurtleState] -> [ColouredLine]
    trace' [] x xs = []
    trace' (s' : st') lsat (sat : sats)
      | s' == 'F' = ((x,y),(x',y'),col) : trace' st' nsat (sat : sats)
      | s' == '[' = trace' st' lsat (lsat : sat : sats)
      | s' == ']' = trace' st' sat sats 
      | otherwise = trace' st' nsat (sat : sats)
        where
        ((x', y') , b') = move s' ((x , y) , b) a
        ((x , y) , b ) = lsat
        nsat = ((x', y') , b')
   

-- enhanced version of trace, takes a list of initial strings         
-- traces multiple objects.
mTrace :: [String] -> Float -> Colour -> [ColouredLine]
mTrace [] a col = [((0,0),(0,0),col)]
mTrace st a col = concat (mTrace' st)
  where
  mTrace' :: [String] -> [[ColouredLine]]
  mTrace' [] = []
  mTrace' (s' : st') = trace s' a col : mTrace' st'
   

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem :: System -> Int -> Colour -> IO ()
drawLSystem system n colour
  = drawLines (trace (lSystem system n) (angle system) colour)
