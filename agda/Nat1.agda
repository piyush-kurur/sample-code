module Nat1 where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero   + b = b
succ a + b = succ (a + b)

open import Equality
one   = succ zero
two   = succ one
three = succ two

0-is-id : ∀ (n : ℕ) → (n + zero) ≡ n
0-is-id zero     =
  begin
    (zero + zero) ≈ zero by definition
  ∎
0-is-id (succ y) =
  begin
    (succ y + zero) ≈ succ (y + zero) by definition
                    ≈ succ y          by cong succ (0-is-id y)
  ∎

+-assoc : ∀ (a b c : ℕ) → (a + b) + c ≡ a + (b + c)
+-assoc zero     b c = definition
+-assoc (succ a) b c =
  begin ((succ a + b) + c)
               ≈ succ (a + b) + c by definition
               ≈ succ ((a + b) + c) by definition
               ≈ succ (a + (b + c)) by cong succ (+-assoc a b c)
               ≈ succ a + (b + c) by definition
  ∎
{-
+-assoc zero     b c     = definition
+-assoc (succ a) b c     =
  begin ((succ a + b) + c)
               ≈ succ (a + b)  + c   by definition
               ≈ succ ((a + b) + c)  by definition
               ≈ succ (a  + (b + c)) by cong succ (+-assoc a b c)
               ≈ succ a   + (b + c)  by definition
  ∎
-}
