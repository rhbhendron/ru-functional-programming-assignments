module ReverseCat where

open import Relation.Binary.PropositionalEquality hiding ([_])
open import Tactic.Cong using (cong!)
open import Data.List using (List; _∷_; []; [_])
open ≡-Reasoning

postulate
  A : Set

  -- (++) : [a] -> [a] -> [a]
  -- [] ++ ys = ys
  -- (x:xs) ++ ys = x : (xs ++ ys)
  _++_ : List A → List A → List A
  ++-def-1 : ∀ {ys} → [] ++ ys ≡ ys
  ++-def-2 : ∀ {x xs ys} → (x ∷ xs) ++ ys ≡ x ∷ (xs ++ ys)


-- The following were proved in exercise 8.1

++-left-identity  : ∀ {ys} → [] ++ ys ≡ ys
++-left-identity {ys} =
  begin
    [] ++ ys
  ≡⟨ ++-def-1 ⟩
    ys
  ∎ 


++-right-identity : ∀ {xs} → xs ++ [] ≡ xs
++-right-identity {[]} =
  begin
    [] ++ []
  ≡⟨ ++-def-1 ⟩
    []
  ∎
++-right-identity {x ∷ xs} = 
  begin
    (x ∷ xs) ++ []
  ≡⟨ ++-def-2 ⟩
    x ∷ (xs ++ [])
  ≡⟨ cong! ++-right-identity ⟩ -- inductive step
    x ∷ xs
  ∎


++-associative : ∀ {xs ys zs} → xs ++ (ys ++ zs) ≡ (xs ++ ys) ++ zs
++-associative {[]} {ys} {zs} =
  begin
    [] ++ (ys ++ zs)
  ≡⟨ ++-def-1 ⟩
    ys ++ zs
  ≡⟨ cong! (sym ++-def-1) ⟩
    ([] ++ ys) ++ zs
  ∎
++-associative {x ∷ xs} {ys} {zs} = 
  begin
    (x ∷ xs) ++ (ys ++ zs)
  ≡⟨ ++-def-2 ⟩
    x ∷ (xs ++ (ys ++ zs))
  ≡⟨ cong (_∷_ x) (++-associative {xs} {ys} {zs}) ⟩
    x ∷ ((xs ++ ys) ++ zs)
  ≡⟨ sym ++-def-2 ⟩
    (x ∷ (xs ++ ys)) ++ zs
  ≡⟨ cong! (sym ++-def-2) ⟩
    ((x ∷ xs) ++ ys) ++ zs
  ∎


-- Now proofs about reverseCat

postulate
  -- reverse :: [a] -> [a]
  -- reverse [] = []
  -- reverse (x:xs) = reverse xs ++ [x]
  reverse : List A → List A
  reverse-def-1 : reverse [] ≡ []
  reverse-def-2 : ∀ {x xs} → reverse (x ∷ xs) ≡ reverse xs ++ [ x ]

  -- reverseCat :: [a] -> [a] -> [a]
  -- reverseCat [] ys = ys
  -- reverseCat (x:xs) ys = reverseCat xs (x:ys)
  reverseCat : List A → List A → List A
  reverseCat-def-1 : ∀ {ys} → reverseCat [] ys ≡ ys
  reverseCat-def-2 : ∀ {x xs ys} → reverseCat (x ∷ xs) ys ≡ reverseCat xs (x ∷ ys)

  -- reverse' :: [a] -> [a]
  -- reverse' xs = reverseCat xs []
  reverse' : List A → List A
  reverse'-def-1 : ∀ {xs} → reverse' xs ≡ reverseCat xs [] 


prop-reverseCat-reverse : ∀ xs ys → reverseCat xs ys ≡ reverse xs ++ ys
prop-reverseCat-reverse xs ys = {!!}

prop-reverse'-reverse : ∀ xs → reverse' xs ≡ reverse xs
prop-reverse'-reverse xs = {!!}
