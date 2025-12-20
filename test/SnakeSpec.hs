module SnakeSpec (spec) where

import Test.Hspec
import Game.Snake
import Scratchy.Syntax
import Graphics.Gloss.Interface.Pure.Game

spec :: Spec
spec = do
  describe "Game.Snake" $ do
    describe "isOpposite" $ do
      it "returns True for U and D" $ do
        isOpposite U D `shouldBe` True
      it "returns True for D and U" $ do
        isOpposite D U `shouldBe` True
      it "returns True for L and R" $ do
        isOpposite L R `shouldBe` True
      it "returns True for R and L" $ do
        isOpposite R L `shouldBe` True
      it "returns False for U and L" $ do
        isOpposite U L `shouldBe` False
      it "returns False for R and U" $ do
        isOpposite R U `shouldBe` False

    describe "Key Bindings" $ do
      it "Player 1 (Green) has correct keys" $ do
        let keys = player1Keys
        lookup (Char 'w') keys `shouldBe` Just U
        lookup (Char 's') keys `shouldBe` Just D
        lookup (Char 'a') keys `shouldBe` Just L
        lookup (Char 'd') keys `shouldBe` Just R
        length keys `shouldBe` 4

      it "Player 2 (Blue) has correct keys" $ do
        let keys = player2Keys
        lookup (Char 'i') keys `shouldBe` Just U
        lookup (Char 'k') keys `shouldBe` Just D
        lookup (Char 'j') keys `shouldBe` Just L
        lookup (Char 'l') keys `shouldBe` Just R
        length keys `shouldBe` 4

    describe "Game Structure" $ do
      it "creates exactly 16 sprites (2 snakes * (1 head + 7 body segments))" $ do
        countSprites snake `shouldBe` 16

-- Tæller antallet af sprites i et SProg
countSprites :: SProg a -> Int
countSprites (Pure _) = 0
countSprites (Combine p1 p2) = countSprites p1 + countSprites p2
countSprites (NewSprite _ _ cont) = 1 + countSprites (cont 0)
countSprites (OnKeyEvent _ _ rest) = countSprites rest
countSprites (OnTargetReached _ _ rest) = countSprites rest
countSprites (OnTargetUpdated _ _ rest) = countSprites rest
countSprites (OnBarrierHit _ _ rest) = countSprites rest
countSprites (SetColor _ _ rest) = countSprites rest
countSprites (SetTarget _ _ rest) = countSprites rest
countSprites (GetTarget _ cont) = countSprites (cont (0, 0))
countSprites (SetBackgroundColor _ rest) = countSprites rest
countSprites (InspectCell _ cont) = countSprites (cont IsFree)
countSprites (After _ _ rest) = countSprites rest
