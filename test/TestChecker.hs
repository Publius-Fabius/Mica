{-# LANGUAGE OverloadedStrings #-}

module TestChecker where

import Control.Monad.State
import Control.Monad.Except
import Text.Megaparsec
import Mica.Type
import Mica.Checker 
import Test.Hspec
import Data.Map 
import Data.Text

ip = initialPos ""

arr = Bin ip "->"
tv = TVar ip 
iden = Iden ip 

mySt = ChkSt { 
    env = Data.Map.fromList [
        
        -- (a -> b) -> (a -> b)
        (" ", ((tv "a" `arr` tv "b") `arr` (tv "a" `arr` tv "b"))),
        
        -- n -> (n -> n)
        ("+", (tv "n" `arr` (tv "n" `arr` tv "n"))),

        ("!", tv "y" `arr` tv "y"),
        ("?", tv "x" `arr` (Bin ip " " (iden "Maybe") (tv "x"))),
        ("var", iden "IntLike")
    ],
    ctx = Data.Map.empty,
    retTy = Nothing }

stripEx p = const () <$> p

shouldChk ma b = case runExcept $ runStateT ma mySt of 
    Right (a, st) -> do 
     --   putStrLn $ show st 
        (stripEx a) `shouldBe` b
    Left err -> error (unpack err)

test1 = do
    it "type checks a basic int" $ 
        cTy (IntLit ip 3) `shouldChk` (Iden () "IntLike")
    it "type checks an operator in the enviroment" $
        cTy (Iden ip "!") `shouldChk` Bin () "->" (TVar () "y") (TVar () "y")
    it "type checks simple unary application" $ 
        cTy (Una ip "!" (Iden ip "var")) `shouldChk` (Iden () "IntLike")
    it "type checks partial application" $ 
        cTy (Una ip "+" (Iden ip "var")) `shouldChk` 
        (Bin () "->" (Iden () "IntLike") (Iden () "IntLike"))
    it "it simplifies a unary with a kind variable" $ 
        cTy (Una ip "?" (Iden ip "var")) `shouldChk` 
        (Bin () " " (Iden () "Maybe") (Iden () "IntLike"))
    it "type checks a one app step" $ 
        cTy (Una ip " " (Iden ip "+")) `shouldChk`
        (Bin () "->" (TVar () "n") (Bin () "->" (TVar () "n") (TVar () "n")))
    it "type checks a full app" $ 
        cTy (Bin ip " " (Iden ip "+") (Iden ip "var")) `shouldChk` 
        Bin () "->" (Iden () "IntLike") (Iden () "IntLike")
    it "type checks a lambda" $ 
        cTy (Lam ip [
                Arg (Name ip "z") Nothing,
                Arg (Name ip "p") Nothing
                ] $ 
            Compound [Ret ip $ Just $ Iden ip "var"]) 
        `shouldChk` 
        (TLam () (
            Bin () "->" (TVar () "x0") (Bin () "->" (TVar () "x1") (Iden () "IntLike"))))
    

test :: IO () 
test = hspec test1