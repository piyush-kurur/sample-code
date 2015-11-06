module Nat where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

_+_ : ℕ → ℕ → ℕ
zero   + b = b
succ a + b = succ (a + b)

_×_ : ℕ → ℕ → ℕ
zero   × b = zero
succ a × b = (a × b) + b


open import Relation.Binary.PropositionalEquality

0-is-right-identity-of-+ : ∀ (n : ℕ) → n + zero ≡ n
0-is-right-identity-of-+ zero     = refl
0-is-right-identity-of-+ (succ n) = cong succ (0-is-right-identity-of-+ n)


+-is-associative : ∀ (a b c : ℕ) → a + (b + c) ≡ (a + b) + c
+-is-associative zero     b c = refl
+-is-associative (succ a) b c = cong succ (+-is-associative a b c)


lemma : ∀ (a b : ℕ) → a + succ b ≡ succ (a + b)
lemma zero     b = refl
lemma (succ a) b = cong succ (lemma a b)

import Relation.Binary.EqReasoning as EqR
open module EqNat = EqR (setoid ℕ)

+-is-commutative : ∀ (a b : ℕ) → a + b ≡ b + a
+-is-commutative a zero     = 0-is-right-identity-of-+ a
+-is-commutative a (succ b) =
  begin
    a + succ b
      ≈⟨ lemma a b ⟩
    succ (a + b)
      ≈⟨ cong succ (+-is-commutative a b) ⟩
    succ (b + a)
      ≈⟨ refl ⟩
    succ b + a
  ∎
