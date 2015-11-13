-- This is an implementation of the equality type for Sets. Agda's
-- standard equality is more powerful. The main idea here is to
-- illustrate the equality type.
module Equality where

open import Level
-- The equality of two elements of type A. The type a ≡ b is a family
-- of types which captures the statement of equality in A. Not all of
-- the types are inhabited as in general not all elements are equal
-- here. The only type that are inhabited are a ≡ a by the element
-- that we call definition. This is called refl (for reflection in
-- agda).
data _≡_  {ℓ} {A : Set ℓ} : (a b : A) → Set ℓ where
  refl : {x : A} → x ≡ x

-- ≡ is a symmetric relation.
sym : ∀{ℓ} {A : Set ℓ} {a b : A} → a ≡ b → b ≡ a
sym refl = refl

-- ≡ is a transitive relation.
trans : ∀ {ℓ} {A : Set ℓ} {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans pAB refl = pAB
-- trans refl pBC = pBC  -- alternate proof of transitivity.

-- Congruence. If we apply f to equals the result are also equal.
cong : ∀{ℓ₀ ℓ₁} {A : Set ℓ₀} {B : Set ℓ₁}
       {a₀ a₁ : A} → (f : A → B) → a₀ ≡ a₁ → f a₀ ≡ f a₁
cong f refl = refl

-- Pretty way of doing equational reasoning.  If we want to prove a₀ ≡
-- b through an intermediate set of equations use this. The general
-- form will look like.
--
-- begin a  ≈ a₀ by p₀
--          ≈ a₁ by p₁
--          ...
--          ≈ b  by p
-- ∎

begin : ∀{ℓ} {A : Set ℓ} (a : A) →  a ≡ a
_≈_by_ : ∀{ℓ} {A : Set ℓ} {a b : A} → a ≡ b → (c : A) → b ≡ c → a ≡ c
_∎ : ∀{ℓ} {A : Set ℓ} (a : A) → A

begin a = refl
aEb ≈ c by bEc = trans aEb bEc
x ∎ = x

infixl 1 _≈_by_
infixl 1 _≡_
