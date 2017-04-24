{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import           Control.Applicative
import           Control.Monad              (join)
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import           Data.Char                  (isSpace)
import           Data.Text                  (Text)

integer :: Parser Integer
integer = do
  skipSpace
  x <- decimal
  skipSpace
  return x

float :: Parser String
float = do
  skipSpace
  (x :: Integer) <- decimal
  char '.'
  (y :: Integer) <- decimal
  skipSpace
  return $ (show x) ++ "." ++ (show y)

dotted :: Parser [Integer]
dotted = do
  skipSpace
  (x :: Integer) <- decimal 
  xs <- some $ do
    char '.'
    decimal
  skipSpace
  return (x:xs)

number :: Parser [Integer]
number = do
  skipSpace
  (x :: Integer) <- decimal 
  xs <- many $ do
    char '.'
    decimal
  skipSpace
  return (x:xs)

containnumber :: Parser String
containnumber = do
  skipSpace
  x <- many $ do
    satisfy (notInClass "0123456789")
  yzs <- some $ do
    y <- some $ do
      satisfy (inClass "0123456789")
    z <- many $ do 
      satisfy (notInClass "0123456789")
    return (y ++ z)
  
  skipSpace
  return (x ++ (join yzs))

section :: Parser [String]
section = do
  string "Section"
  skipSpace
  x <- dotted
  return (["Section"] ++ (map show x))

dictItem :: Parser (Text,[Text])
dictItem = do
  skipSpace
  x <- AT.takeWhile (isSpace) 
  string "->"
  skipSpace
  xs <- some $ do
    AT.takeWhile (\c -> if (c == ',') then False else True)
    AT.take 1
  skipSpace 
  return (x,xs)

dictionary :: Parser ([Text])
dictionary = do
  skipSpace
  x <- AT.takeWhile (not . isSpace)
  skipSpace
  string "->"
  skipSpace
  y <- AT.takeWhile (\c -> if (c == ',') then False else True)
  xys <- some $ do
    string ","
    AT.takeWhile (\c -> if (c == ',' || c == '\n') then False else True)
    
  skipSpace

  --- (\x -> if (x==(' ' :: Char)) then False else True)
  {-
  string "->"
  skipSpace
  xs <- some $ do
    AT.takeWhile (\c -> if (c == ',') then False else True)
    AT.take 1
  skipSpace
  -}
  return (x:y:xys)
