{-***********************************
dominoes.hs
Dominoes assignment game (PART 2)
************************************-}
module Dominoes where

 --Imports for the modules used
 import System.Random
 import MergeSort
 import Data.List ((\\))
 import Debug.Trace

 --Data structure for a domino represented as a tuple.
 type Dom = (Int,Int)
 
 --Data structure for a board and hand which must contain a list of dominoes.
 type Hand = [Dom]

 type Board = [Dom] 

 --Type representing an end of a board.
 data End = L|R 
  deriving (Eq,Show)

 --type representing a move, consisting of the dom to place and the end to place it at
 type Move = (End,Dom)

 --Data structure representing a domsplayer, takes a board and a hand and returns a move
 type DomsPlayer =  Board -> Hand -> Move

 --A constant representing the complete set of dominoes using a comprehension
 domSet :: [Dom]
 domSet = [(n,m)|n<-[0..6],m<-[0..6], m<=n]

 -------------------------------------------------
 --Random hands

 --takes a seed value and returns a randomised list of the domino set
 shuffleDoms :: Int -> [Dom]
 shuffleDoms seed =
   let
   --returns a list of random numbers
   rlis = take 28 (randoms (mkStdGen seed):: [Int])
   --zips the random numbers with the domset, producing a lsit of (dom,Int) pairs
   zlis = zip domSet rlis
   --sort the list according to the ints values
   slis = mergesort (\(_,n1) (_,n2)->n1<n2) zlis
   in
   --use map to extract the dominoes in random order
   map fst slis
  
 --Returns a hand of nine dominoes taken from the randomised domino set 
 defineHand1 :: Int -> Hand
 defineHand1 seed =  take 9 $ shuffleDoms(seed)

 --Returns a hand containing the next nine dominoes from the randomised domino set 
 defineHand2 :: Int -> Hand
 defineHand2 seed = drop 9 . take 18 $ shuffleDoms(seed)
 
 -------------------------------------------------
 --Highest scoring dom player
 
 --A player returning the highest scoring domino, or otherwise plays the first playable dominoe 
 --checking the left side first
 hsdPlayer :: DomsPlayer
 hsdPlayer brd hnd
   |goesright/=[] && goesleft/=[] = highestscoring --if there are doms that go right and left
   |goesright==[] && goesleft/=[] = (L,lefthighest) --if there are doms that go only left
   |goesright/=[] && goesleft==[] = (R,righthighest)--if there are doms that go only right 
   where
   goesright = rightdrops hnd brd --list of doms that go right
   goesleft = leftdrops hnd brd --list of doms that go left
   righthighest = getHighestAtEnd R brd goesright 0 (head goesright) --highest dom that goes right
   lefthighest = getHighestAtEnd L brd goesleft 0 (head goesleft) -- --highest dom that goes left
   highestscoring = compareHighest lefthighest righthighest brd -- highest of the two highest doms
  
 --takes an end, board and hand and returns the highest scoring dom 
 --recurses through, keeping track of the highest scoring dom and its score
 getHighestAtEnd :: End -> Board -> Hand -> Int -> Dom -> Dom
 getHighestAtEnd _ _ [] _ dom = dom
 getHighestAtEnd end brd (h:t) score highestDom
   --if the next dom in the list reults in a higher score, recurses with the new score and dom
   |newscore>score = getHighestAtEnd end brd t newscore newHighestDom
   --else recurses with the current highest dom and its score
   |otherwise = getHighestAtEnd end brd t score highestDom
   where 
   newscore = scoreBoard (updateBoard (end,h) brd)
   newHighestDom= h

 --takes two dominoes and a board and returns the move that results in the highest score
 compareHighest :: Dom -> Dom -> Board -> Move
 --used to get the move based on the right highest dom and left highest dom by comparing them
 compareHighest dom1 dom2 brd
   |(scoreBoard (updateBoard (L, dom1) brd)) > (scoreBoard (updateBoard (R, dom2) brd)) = (L,dom1)
   |otherwise = (R,dom2)

 -------------------------------------------------
 --simple player
 
 --takes a board and a hand and returns the move of the first playable domino
 simplePlayer :: DomsPlayer
 --recurses through the hand checking if each dom is playable, if playable then returns end and dom
 simplePlayer brd (h:t)
   |(goes_left) = (L,h)
   |(goes_right) = (R,h)
   |otherwise = simplePlayer brd t
   where
   goes_left = goesLP h brd
   goes_right = goesRP h brd
   
 -------------------------------------------------
 --play game
 
 --Plays a game of fives and threes dominoes between two simple domsplayers, returns accumulated scores, 
 --e.g. (player1 score, player2 score)
 playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int,Int)
 playDomsRound player1 player2 seed =
   let
   --sets the initial hands of the two players using a randomised seed
   initialhand1 = defineHand1 seed
   initialhand2 = defineHand2 seed
   initialboard=[] -- sets initial board
   initialscore=0 --sets the initial scores for both players
   in
   play initialhand1 initialhand2 initialboard player1 player2 initialscore initialscore
  
 {-This function recurses until both domplayers are knocking, it keeps track of each players hand and score 
  as well as the current state of the board. Returns accumulated scores of both players-}
 play :: Hand -> Hand -> Board -> DomsPlayer -> DomsPlayer -> Int -> Int -> (Int,Int)
 play hnd hnd2 brd player player2 score score2
   |(knockingP hnd brd) && (knockingP hnd2 brd) =(score,score2)
   --recurses using both players newhands, scores and new board after they both take a turn
   -- e.g. if one player can't go,their hand and score remains the same
   |otherwise = play newhand newhand2 newboard2 player player2 totalscore totalscore2
   where
   --simulates player 1 turn
   move = getMove hnd brd player
   --board and hand updated if move is valid
   newhand = updateHand move hnd
   newboard = updateBoard move brd
   --players score updated
   totalscore = updatePlayerScore hnd newboard score
   
   --simulates player2 turn
   move2 = getMove hnd2 newboard player2
   newhand2 =  updateHand move2 hnd2  
   newboard2 = updateBoard move2 newboard
   totalscore2 = updatePlayerScore hnd2 newboard2 score2 
   
  
 --takes a hand, board and domplayer and gets their move
 --checks if they are knocking
 getMove ::  Hand -> Board -> DomsPlayer -> Move
 getMove hnd brd player
   |(knockingP hnd brd)==False = player brd hnd
   {-if knocking, sentinel value returned since this will not affect the players hand or the board
   when they are updated, and so simulates skipping their turn (see above)-}
   |otherwise = (L,(7,7))
   
 --takes a hand, board, domplayer and score and returns the new accumulated score following a players move
 updatePlayerScore :: Hand -> Board -> Int -> Int
 updatePlayerScore hnd brd score
 --returns current score if they are knocking or new score if player can play
   |(knockingP hnd brd) = score
   |otherwise = score+scoreBoard brd
 
  -------------------------------------------------
  --update board and hand

 --takes a dom an end and a board and returns new board if dom will go at end accounts for domino being flipped
 updateBoard ::  Move -> Board -> Board
 updateBoard (end,(d1,d2)) [] = [(d1,d2)]
 updateBoard (end,(d1,d2)) brd
   |(d2==fst (head brd)) && (end==L) = ((d1,d2):brd)
   |(d1==fst (head brd)) && (end==L) = ((d2,d1):brd)
   |(d1==snd (last brd)) && (end==R) = (brd++[(d1,d2)])
   |(d2==snd (last brd)) && (end==R) = (brd++[(d2,d1)])
   |otherwise = brd
  
  {-takes a move and a hand and returns new hand after removing dom used in move from the hand.
  Does not need to account for dom in move being flipped since dom in move will always be same
  orientation as it is in the hand -}
 updateHand :: Move -> Hand-> Hand
 updateHand (m1,m2) hnd = 
   let 
   a = [m2]
   in
   hnd \\ a -- uses data.lsit
   
   {-------------------------------------------------------
                 CODE FROM ASSIGNMENT 1
   -------------------------------------------------------} 
   --Can a domino be played?
   
   --takes a board and returns its end values, left end first then right e.g. (L,R)
 getEnds :: Board->(Dom,Dom)
 getEnds b = (head b,last b)
 
 --takes a domino and a board and returns true or false as to wether it can be played left
 goesLP :: Dom->Board->Bool
 goesLP _ [] = True
 goesLP (d1,d2) b = (l==d1)||(l==d2)
   where ((l,_),_) = getEnds b
 
 --takes a domino and a board and returns true or false as to wether it can be played right
 goesRP :: Dom->Board->Bool
 goesRP _ [] = True
 goesRP (d1,d2) b = (r==d1)||(r==d2)
   where (_,(_,r)) = getEnds b
   
  --------------------------------------------------------  
  --Is player knocking? (no domino can be played)
  
 -- knockingP
 -- uses possPlays 
 knockingP :: Hand->Board->Bool
 knockingP h b = (null gl)&& (null gr)
   where (gl,gr)=possPlays h b
   
  --------------------------------------------------------
  --Get list of dominoes that go right or left
  
 -- possPlays
 -- return (left plays, right plays)
 possPlays :: Hand->Board->(Hand,Hand)
 possPlays h b = (leftdrops h b, rightdrops h b) 
 
 --takes a hand and board and returns a hand containing the doms playable to the right
 rightdrops :: Hand->Board->Hand
 rightdrops h b = filter (\d -> goesRP d b) h 
 
 --takes a hand and board and returns a hand containing the doms playable to the left
 leftdrops :: Hand->Board->Hand
 leftdrops h b = filter (\d -> goesLP d b) h
 -------------------------------------------------  
 --Get score of board

 -- 5s & threes score for a board
 scoreBoard :: Board -> Int
 --trivial cases
 scoreBoard [] = 0
 scoreBoard [(d1,d2)] = score53 (d1+d2)
 --general case
 scoreBoard b = 
  let
   (lend,rend)=getEnds b
  in
    score53 ((domScore lend L)+ (domScore rend R))
 
 -- allow for doubles 
 domScore :: Dom->End->Int
 domScore (l,r) e 
   |l==r = 2*l
   |e == L = l
   |otherwise = r 
   
 -- 5s and 3s score for a number 
 score53 :: Int->Int
 score53 n
   |n==3 = 1
   |n==5 = 1
   |n==6 = 2
   |n==9 = 3
   |n==10=2
   |n==12 = 4
   |n ==15 = 8
   |n==18= 6
   |n==20 = 4
   |otherwise = 0  
 