module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (an , ax , r)
    = an

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (an , ax , r)
    = ax

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (an , ax , r)
    = r

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar char r
    = head [y | (x , y) <- r , x == char]

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne [] _          = []
expandOne (x : xs) r    = concat ((lookupChar x r) : [expandOne xs r])

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand command 0 _      = command
expand command num r    = expand (expandOne command r) (num - 1) r

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move 'L' an' ((x , y) , an)     = ((x , y) , an + an')
move 'R' an' ((x , y) , an)     = ((x , y) , an - an')
move 'F' _ ((x , y) , an)       = ((x + (cos deg) , y + (sin deg)), an)
    where deg = an / 180 * pi

--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--

-- Traces lines based on the recursion method
trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 command an col
    = finaltrace
    where
        (finaltrace , _) = trace1' command an col ((0.0 , 0.0) , 90.0)
        trace1' :: Commands -> Angle -> Colour -> TurtleState -> ([ColouredLine] , Commands)
        trace1' [] _ _ _        = ([] , "")
        trace1' (x : xs) an col oldpos@(start , an')
            | elem x "LR"                           = (trace , remainder)
            | x == 'F'                              = ((cline : trace) , remainder)
            | x == '['                              = ((trace' ++ trace'') , remainder'')
            | x == ']'                              = ([] , xs)
            where
                newpos@(end , an'')     = move x an oldpos
                cline   = (start , end , col)
                (trace , remainder)     = trace1' xs an col newpos
                (trace' , remainder')   = trace1' xs an col oldpos
                (trace'' , remainder'') = trace1' remainder' an col oldpos



-- Traces lines by using an explicit stack to remember the turtle states
trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 command an col
    = trace2' command an col [] ((0.0 , 0.0) , 90.0)
    where
        trace2' :: Commands -> Angle -> Colour -> Stack -> TurtleState -> [ColouredLine]
        trace2' []  _ _ _ _     = []
        trace2' (x : xs) an col stack oldpos@(start , an')
            | elem x "LR"       = trace2' xs an col stack newpos
            | x == 'F'          = cline : (trace2' xs an col stack newpos)
            | x == '['          = trace2' xs an col stack' oldpos
            | x == ']'          = trace2' xs an col stack'' savedpos
            where
                newpos@(end , an'') = move x an oldpos
                cline   = (start , end , col)
                stack'  = oldpos : stack
                savedpos : stack'' = stack

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
