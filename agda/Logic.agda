module Logic where

-- The true proposition.
data ⊤ : Set where
  obvious : ⊤ -- The proof of truth.

-- The false proposition.
data ⊥ : Set where
  -- There is nothing here so one can never prove false.

-- The AND of two statments.
data _∧_ (A B : Set)  : Set where
  -- The only way to construct a proof of A ∧ B is by pairing a a
  -- proof of A with a proof of and B.
  ⟨_,_⟩ : (a : A)  -- Proof of A
        → (b : B)  -- Proof of B
        → A ∧ B    -- Proof of A ∧ B

-- The OR of two statements.
data _∨_ (A B : Set) : Set where
  -- There are two ways of constructing a proof of A ∨ B.
  inl : (a : A) →  A ∨ B   -- From a proof of A by left introduction
  inr : (b : B) →  A ∨ B   -- From a proof of B by right introduction

-- The not of statement A
¬_ : (A : Set) → Set
¬ A = A → ⊥  -- Given a proof of A one should be able to get a proof
             -- of ⊥.

-- The statement A ↔ B are equivalent.
_↔_ : (A B : Set) → Set
A ↔ B = (A → B) -- If
           ∧    -- and
        (B → A) -- only if


infixr 1 _∧_
infixr 1 _∨_
infixr 0 _↔_
infix  2 ¬_

-- Function composition
_∘_   : {A B C : Set} → (B → C) → (A → B) → A → C
(f ∘ g) x = f (g x)

-- Double negation
doubleNegation : ∀ {A : Set} → A → ¬ (¬ A)
doubleNegation a negNegA = negNegA a

{-

doubleNegation' : ∀ {A : Set} → ¬ ( ¬ (¬ A)) → ¬ A

-}


deMorgan1 : ∀ (A B : Set) → ¬ (A ∨ B) → ¬ A ∧ ¬ B
deMorgan1 A B notAorB = ⟨ notAorB ∘ inl , notAorB ∘ inr ⟩

deMorgan2 : ∀ (A B : Set) → ¬ A ∧ ¬ B → ¬ (A ∨ B)
deMorgan2 A B ⟨ notA , notB ⟩ (inl a) = notA a
deMorgan2 A B ⟨ notA , notB ⟩ (inr b) = notB b


deMorgan : ∀ (A B : Set) → ¬ (A ∨ B) ↔ ¬ A ∧ ¬ B
deMorgan A B = ⟨ deMorgan1 A B , deMorgan2 A B ⟩
