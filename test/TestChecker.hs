{-# LANGUAGE OverloadedStrings #-}

module TestChecker where

import Control.Monad.State
import Control.Monad.Except
import Text.Megaparsec
import Mica.Cute
import Mica.Type
import Mica.Checker 
import Test.Hspec
import Data.Map 
import Data.Text

ip = initialPos "TestChecker.hs"

arr = Bin () "->"
tv = TVar () 
iden = Iden () 

mySt = ChkSt { 
    environment = Data.Map.fromList [
        (" ", ((tv "a" `arr` tv "b") `arr` (tv "a" `arr` tv "b"))),
        ("+", (tv "n" `arr` (tv "n" `arr` tv "n"))),
        ("!", tv "y" `arr` tv "y"),
        ("?", tv "x" `arr` (Bin () " " (iden "Maybe") (tv "x"))),
        ("var", iden "IntLike")
    ],
    Mica.Checker.context = Data.Map.fromList [
    ],
    returns = Nothing,
    uid = 0 }

checks :: [Exp ()] -> Exp SP -> Checker (Exp Note)
checks es ex = do 
    mapM_ freshen es 
    checkExp ex  

stripEx p = const () <$> p

shouldChk ma b = case runExcept $ runStateT ma mySt of 
    Right (ex, st) -> do 
        let ps = unpack $ Data.Text.intercalate "\n" (cute <$> problems ex)
        case ps of 
            [] -> pure () 
            _ -> putStrLn "" >> putStrLn ps
        typeof ex `shouldBe` b
    Left err -> error (unpack err)

test1 = do
    it "type checks a basic int" $ 
        checkExp (IntLit ip 3) `shouldChk` (Iden () "IntLike")
    it "type checks an operator in the enviroment" $
        checks [] 
        (Iden ip "!") `shouldChk` Bin () "->" (TVar () "y") (TVar () "y")
    it "freshens an operators parameters when introduced into the context" $
        checks [tv "y"] 
        (Iden ip "!") `shouldChk` Bin () "->" (TVar () "y0") (TVar () "y0")
    it "type checks simple unary application" $ 
        checks [] (Una ip "!" (Iden ip "var")) `shouldChk` (Iden () "IntLike")
    it "type checks partial application" $ 
        checks [] (Una ip "+" (Iden ip "var")) `shouldChk` 
        (Bin () "->" (Iden () "IntLike") (Iden () "IntLike"))
    it "it simplifies a unary with a kind variable" $ 
        checks [] (Una ip "?" (Iden ip "var")) `shouldChk` 
        (Bin () " " (Iden () "Maybe") (Iden () "IntLike"))
    it "type checks a one app step" $ 
        checks [] (Una ip " " (Iden ip "+")) `shouldChk`
        (Bin () "->" (TVar () "n") (Bin () "->" (TVar () "n") (TVar () "n")))
    it "type checks a full app" $ 
        checks [] (Bin ip " " (Iden ip "+") (Iden ip "var")) `shouldChk` 
        Bin () "->" (Iden () "IntLike") (Iden () "IntLike")
    it "type checks a lambda" $ 
        checks [] (Lam ip [
                Arg (Name ip "z") Nothing,
                Arg (Name ip "p") Nothing
                ] $ 
            Compound ip [Ret ip $ Just $ Iden ip "var"]) 
        `shouldChk` 
        Bin () "-->" (Bin () "," (TVar () "x") (TVar () "x0")) (Iden () "IntLike")
 
test :: IO () 
test = hspec test1