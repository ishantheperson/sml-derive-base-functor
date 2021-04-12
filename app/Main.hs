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

  print $ printDataType functor
  print $ printFunction projectFunc 
  print $ printFunction embedFunc
  print $ printFunction mapFunc 