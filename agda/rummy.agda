module rummy where

open import Data.Nat

-- The suit type
data Suit : Set where
  ♣ : Suit
  ♢ : Suit
  ♥ : Suit
  ♠ : Suit

-- A card consists of a value and a suite.
data Card : ℕ → Suit → Set where
  _of_ : (n : ℕ) (s : Suit) → Card n s

-- Common names for cards

A : ℕ; A = 1
K : ℕ; K = 13
Q : ℕ; Q = 12
J : ℕ; J = 11


--
-- The clubs suit
--

A♣  : Card A  ♣; A♣  = A  of ♣
2♣  : Card 2  ♣; 2♣  = 2  of ♣
3♣  : Card 3  ♣; 3♣  = 3  of ♣
4♣  : Card 4  ♣; 4♣  = 4  of ♣
5♣  : Card 5  ♣; 5♣  = 5  of ♣
6♣  : Card 6  ♣; 6♣  = 6  of ♣
7♣  : Card 7  ♣; 7♣  = 7  of ♣
8♣  : Card 8  ♣; 8♣  = 8  of ♣
9♣  : Card 9  ♣; 9♣  = 9  of ♣
10♣ : Card 10 ♣; 10♣ = 10 of ♣
J♣  : Card J  ♣; J♣  = J  of ♣
Q♣  : Card Q  ♣; Q♣  = Q  of ♣
K♣  : Card K  ♣; K♣  = K  of ♣

--
-- The diamond suit
--

A♢  : Card A  ♢; A♢  = A  of ♢
2♢  : Card 2  ♢; 2♢  = 2  of ♢
3♢  : Card 3  ♢; 3♢  = 3  of ♢
4♢  : Card 4  ♢; 4♢  = 4  of ♢
5♢  : Card 5  ♢; 5♢  = 5  of ♢
6♢  : Card 6  ♢; 6♢  = 6  of ♢
7♢  : Card 7  ♢; 7♢  = 7  of ♢
8♢  : Card 8  ♢; 8♢  = 8  of ♢
9♢  : Card 9  ♢; 9♢  = 9  of ♢
10♢ : Card 10 ♢; 10♢ = 10 of ♢
J♢  : Card J  ♢; J♢  = J  of ♢
Q♢  : Card Q  ♢; Q♢  = Q  of ♢
K♢  : Card K  ♢; K♢  = K  of ♢

--
-- The heart suit
--

A♥  : Card A  ♥; A♥  = A  of ♥
2♥  : Card 2  ♥; 2♥  = 2  of ♥
3♥  : Card 3  ♥; 3♥  = 3  of ♥
4♥  : Card 4  ♥; 4♥  = 4  of ♥
5♥  : Card 5  ♥; 5♥  = 5  of ♥
6♥  : Card 6  ♥; 6♥  = 6  of ♥
7♥  : Card 7  ♥; 7♥  = 7  of ♥
8♥  : Card 8  ♥; 8♥  = 8  of ♥
9♥  : Card 9  ♥; 9♥  = 9  of ♥
10♥ : Card 10 ♥; 10♥ = 10 of ♥
J♥  : Card J  ♥; J♥  = J  of ♥
Q♥  : Card Q  ♥; Q♥  = Q  of ♥
K♥  : Card K  ♥; K♥  = K  of ♥

--
-- The spade suit
--

A♠  : Card A  ♠; A♠  = A  of ♠
2♠  : Card 2  ♠; 2♠  = 2  of ♠
3♠  : Card 3  ♠; 3♠  = 3  of ♠
4♠  : Card 4  ♠; 4♠  = 4  of ♠
5♠  : Card 5  ♠; 5♠  = 5  of ♠
6♠  : Card 6  ♠; 6♠  = 6  of ♠
7♠  : Card 7  ♠; 7♠  = 7  of ♠
8♠  : Card 8  ♠; 8♠  = 8  of ♠
9♠  : Card 9  ♠; 9♠  = 9  of ♠
10♠ : Card 10 ♠; 10♠ = 10 of ♠
J♠  : Card J  ♠; J♠  = J  of ♠
Q♠  : Card Q  ♠; Q♠  = Q  of ♠
K♠  : Card K  ♠; K♠  = K  of ♠

-- A run of length ℓ.
data Run  (suit : Suit) : (start ℓ : ℕ) → Set where
  [] : {start : ℕ} → Run  suit  start 0

  _,_ : {n ℓ : ℕ}
       → Card n suit
       → Run    suit (1 + n)     ℓ
       → Run    suit    n     (1 + ℓ)


-- A group of length ℓ.
data Group : (value ℓ : ℕ) → Set where
  [] : {value : ℕ} → Group value 0

  _,_ : {suit : Suit} {value ℓ : ℕ}
       → Card   value  suit
       → Group  value    ℓ
       → Group  value    (1 + ℓ)

-- Some pretty functions for creating runs. You can create a run as
-- follows:
--
-- myrun = ⟨ A♣ , 2♣ , 3♣ ⟩
--

⟨_ : {suit : Suit} {n ℓ : ℕ} → Run suit n ℓ → Run suit n ℓ
⟨ r = r

_⟩ : {suit : Suit} {n : ℕ} → Card n suit → Run suit n 1
c ⟩ = c , []


-- Some pretty functions for creating groups. You can create a group
-- group as follows:
--
-- mygroup = ⟦ A♣ , A♥ , A♢ ⟧
--

⟦_ : {value ℓ : ℕ} → Group value ℓ → Group value ℓ
⟦ g = g


_⟧ : {suit : Suit} {n : ℕ} → Card n suit → Group n 1
c ⟧ = c , []

infixr 2 _⟩ _⟧ _,_
infixl 1 ⟨_ ⟦_


mygroup = ⟦ A♣ , A♣ , A♠ ⟧
myrun   = ⟨ A♣ , 2♣ , 3♣ ⟩
