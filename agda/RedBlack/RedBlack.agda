module RedBlack where

open import Data.Nat

-- | The colour of a node.
data Color : Set where
  red   : Color
  black : Color

-- | The red black tree
data Tree  (A : Set) : ℕ       -- black height
                     → Color   -- color of the root
                     → Set
     where

  -- Empty node is always black
  empty :  Tree A 0 black

  -- Both the children of the red node are black.
  red   :  {d : ℕ}
        → Tree A d black  -- left child
        → A               -- element on the node
        → Tree A d black  -- right child
        → Tree A d red    -- The black height does not change

  -- The children can be of either color
  black : {d : ℕ} {cL cR : Color}
        → Tree A d       cL    -- left child
        → A                    -- element
        → Tree A d       cR    -- right child
        → Tree A (d + 1) black -- black height goes up by one.

-- Converts a tree with red root to a black tree. The type encodes the
-- fact that the height of the tree goes up by 1.
blacken : {A : Set} {d : ℕ}
        → Tree A d       red
        → Tree A (d + 1) black
blacken (red lt x rt) = black lt x rt
