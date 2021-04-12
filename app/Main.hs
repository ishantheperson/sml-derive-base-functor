module Main where

import SML.TypeParser
import SML.Generator 
import SML.Print 

main :: IO ()
main = do 
  input <- test <$> getContents

  let 
    functor = baseFunctor input 
    projectFunc = makeProject input 
    embedFunc = makeEmbed input 

    mapFunc = makeMap functor 

  print $ printFunction projectFunc 
  putStrLn ""
  print $ printFunction embedFunc
  putStrLn ""
  print $ printFunction mapFunc 