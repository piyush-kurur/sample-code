module Equality where

data _≡_  {A : Set} : (a b : A) → Set where
  definition : {x : A} → x ≡ x

sym : {A : Set} {a b : A} → a ≡ b → b ≡ a
sym definition = definition

trans : {A : Set} {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans pAB definition = pAB
-- trans definition pBC = pBC

cong : {A B : Set} {a₀ a₁ : A} → (f : A → B) → a₀ ≡ a₁ → f a₀ ≡ f a₁
cong f definition = definition

begin : {A : Set} (a : A) →  a ≡ a
begin a = definition


_≈_by_ : {A : Set} {a b : A} → a ≡ b → (c : A) → b ≡ c → a ≡ c
aEb ≈ c by bEc = trans aEb bEc


_∎ : {A : Set} (a : A) → A
x ∎ = x

infixl 1 _≈_by_
infixl 1 _≡_
