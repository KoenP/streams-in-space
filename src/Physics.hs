module Physics where

import Data.List
import Control.Comonad

import Vec

type Pos          = Vec
type Force        = Vec
type Acceleration = Vec
type Velocity     = Vec
type Mass         = Float
type Length       = Float
type Angle        = Float

-- A Body has a position and a mass.
data Body = Body { bodyPos :: Pos, bodyMass :: Mass }
  deriving (Show, Eq)

-- (body |--) returns the gravitational field of body, that is, it
-- returns a function from positions in space to the force the gravity
-- of the body applies to a unit mass.
-- The field is ill-defined very close to the body.
(|--) :: Body -> (Pos -> Acceleration)
Body pos1 mass |-- pos2 = (mass / dist^2) *^ diff
  where
    diff = pos1 ^-^ pos2
    dist = pos1 `distance` pos2

-- Gravity exerted by the first body on the second.
gravityOnSnd :: Body -> Body -> Force
gravityOnSnd b (Body pos mass) = mass *^ (b |-- pos)

-- Given a list of bodies, return the net gravitational force applied
-- on each of these bodies by all the others.
-- I'm not super hapy with this function, it's a bit messy.
calcBodyForces :: [Body] -> [Force]
calcBodyForces bodies
  = zipWith (^-^) (map sum tailGravities) (map sum $ nibble tailGravities)
  where
    -- Imagine bodies as the list [1,2,3]
    -- Then nonEmptyTails = [[1,2,3], [2,3], [3]]
    nonEmptyTails = takeWhile (not . null) . iterate tail $ bodies

    -- tailGravities = [[g12,g13], [g23], []],
    --   where gij = gravity applied on i by j
    tailGravities = map (\(p:ps) -> map (\p' -> gravityOnSnd p' p) ps) nonEmptyTails


    -- To compute the total gravity on each body, we want to get the list
    -- [(g12+g13) - 0, (g23) - (g12), 0 - (g13+g23)]
    -- Or, in other words, the difference of the lists
    -- [(g12+g13), g23         , 0         ] = map sum tailGravities
    -- [0        , g12         , g13+g23   ] = map sum (nibble tailGravities)

    -- Here nibble is a weird little function that, when given a list
    -- [[1,2,3], [4,5], [6]] as input, produces the list
    -- [[], [1], [2,4], [3,5,6]] as output.
    -- For performance, this implementation expects lists of
    -- monotonically increasing length without checking this
    -- condition, it just crashes if the shape isn't right.
    nibble = go 0
      where go n lists = case splitAt n lists of
              (h,[]) -> map head h : []
              (h, t) -> map head h : go (n+1) (map tail h ++ t)

-- As an aside, here's the same function again, but reimplemented by
-- making use of the Zipper Comonad.
-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- This is a really neat abstraction that collapses our implementation onto
-- just two lines of actual code (here I'm generously not counting the
-- Zipper implementation of course; I think that's fair because it's
-- not specific to this problem at all, list zippers are a useful data
-- structure in general).  Note that unlike the original
-- implementation, this one doesn't re-use already computed forces
-- from earlier in the list, so I expect it to be a bit slower.
-- The workhorse here is the extend function from the Control.Comonad library.
-- I explain it in more detail at the bottom of this file.
calcBodyForcesComonadic :: [Body] -> [Force]
calcBodyForcesComonadic = map (extract . extend force) . zippers
  where
    force :: Zipper Body -> Force
    force z = sumV [gravityOnSnd b (extract z) | b <- others z]

-- A zipper is like a list, but with a "focused" element, and efficient
-- operations to shift the focus.
-- I introduce the notation [1,2,*3,4,5] to mean the zipper `Zipper [2,1] 3 [4,5]`
data Zipper a = Zipper [a] a [a]
  deriving (Show, Eq, Functor)

-- The left and right functions shift the focus one step.
-- It's a bit more involved than the code in the above link because
-- unlike that blog post, we don't assume our lists to be infinite.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _  _ []    ) = Nothing
right (Zipper ls x (r:rs)) = Just (Zipper (x:ls) r rs)

left :: Zipper a -> Maybe (Zipper a)
left  (Zipper []     _ _ ) = Nothing
left  (Zipper (l:ls) x rs) = Just (Zipper ls l (x:rs))

-- Given a list, construct the list of zippers from that list, where
-- each subsequent zipper focuses on the next element of the list.
unfoldr' f = unfoldr (fmap (\x->(x,x)) . f)

zippers :: [a] -> [Zipper a]
zippers []     = []
zippers (a:as) = let z = Zipper [] a as
                 in z : unfoldr' right z

-- Return all elements that are not focused (in a weird order).
others :: Zipper a -> [a]
others (Zipper l _ r) = l ++ r

-- At last, the Comonad instance.
instance Comonad Zipper where
  -- The extract function returns the element currently in focus.
  -- (coreturn in the linked blog post)
  extract (Zipper _ x _) = x

  -- The duplicate :: Zipper a -> Zipper (Zipper a)
  -- (cojoin in the linked blog post)
  -- function turns a zipper into a zipper of zippers.
  -- A `Zipper a` is like a "list" of `a`s with one `a` focused.
  -- A `Zipper (Zipper a)` is then like a "list" of `Zipper a`s with one `Zipper a` focused.
  -- Of course, each of those `Zipper a`s has its own focus.
  -- The zippers generated by duplicate z each focus on a different
  -- element of the original list.
  -- Perhaps best illustrated through an example.
  -- duplicate [1,*2,3] = [  [*1,2,3] , *[1,*2,3] , [1,2,*3]  ]
  duplicate z = Zipper (unfoldr' left z) z (unfoldr' right z)

  -- From these functions, the Comonad library gives us the `extend`
  -- function for free, which will do most of our work.
  -- extend f = fmap f . duplicate
  -- I like to compare `extend` with `fmap`.
  -- fmap   :: Functor f => (a   -> b) -> (f a -> f b)
  -- extend :: Comonad f => (f a -> b) -> (f a -> f b)

  -- In vague terms:

  -- `fmap f` lifts the function `f` that operates on base values into
  -- a function that applies a structure-preserving mapping onto some
  -- structure (the functor). But the value(s) in the structure are considered
  -- in complete isolation, they are stripped of all context before applying `f`.

  -- `extend f` does something similar: it also applies a structure-preserving
  -- mapping by lifting a function `f`. But unlike with `fmap`, our function
  -- is not of type `a -> b` but `f a -> b`; in other words, it does
  -- get to inspect the structure surrounding the value.

  -- In the calcBodyForcesComonadic function, this is exactly what we need:
  -- we want to compute, for each body, its net gravity. This net gravity
  -- depends on the surrounding structure: we can only know it by looking
  -- at the other bodies in the zipper. And finally, our mapping has to
  -- preserve the structure of the zipper.
  
