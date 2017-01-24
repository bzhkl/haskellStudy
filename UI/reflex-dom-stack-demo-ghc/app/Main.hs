{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
--{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where
import           Reflex
import           Reflex.Dom

import qualified Data.Map as Map
import Safe      (readMay)
import Data.Text (pack, unpack, Text)
import Control.Applicative ((<*>), (<$>))



main = mainWidget $ el "div" $ do
  nx <- numberInput
  d <- dropdown "*" (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> textToOp o <$> x <*> y) (_dropdown_value d) values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = "style" =: "border-color: red"
      validState = "style" =: "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
                           & textInputConfig_attributes .~ attrs
      let result = fmap (readMay . unpack) $ _textInput_value n
          attrs  = fmap (maybe errorState (const validState)) result
  return result

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

textToOp :: (Fractional a) => Text -> a -> a -> a
textToOp s = case s of
                    "-" -> (-)
                    "*" -> (*)
                    "/" -> (/)
                    _ -> (+)




-- main = mainWidget $ el "div" $ do
--   nx <- numberInput
--   d <- dropdown "*" (constDyn ops) def
--   ny <- numberInput
--   let values = zipDynWith (,) nx ny
--       result = zipDynWith (\o (x,y) -> textToOp o <$> x <*> y) (_dropdown_value d) values
--       resultText = fmap (pack . show) result
--   text " = "
--   dynText resultText

-- numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = do
--   n <- textInput $ def & textInputConfig_inputType .~ "number"
--                        & textInputConfig_initialValue .~ "0"
--   return . fmap (readMay . unpack) $ _textInput_value n

-- numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = do
--   let attrs = constDyn ("style" =: "border-color: blue")
--   n <- textInput $ def & textInputConfig_inputType .~ "number"
--                        & textInputConfig_initialValue .~ "0"
--                        & textInputConfig_attributes .~ attrs
--   return . fmap (readMay . unpack) $ _textInput_value n
--
-- ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]
--
-- textToOp :: (Fractional a) => Text -> a -> a -> a
-- textToOp s = case s of
--                     "-" -> (-)
--                     "*" -> (*)
--                     "/" -> (/)
--                     _ -> (+)



-- main = mainWidget $ el "div" $ do
--   nx <- numberInput
--   text " + "
--   ny <- numberInput
--   text " = "
--   let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
--       resultString = fmap (pack . show) result
--   dynText resultString
--
-- numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = do
--   n <- textInput $ def & textInputConfig_inputType .~ "number"
--                        & textInputConfig_initialValue .~ "0"
--   return . fmap (readMay . unpack) $ _textInput_value n


-- main :: IO ()
-- main = mainWidget $ do
--     --el "h1" $ text "Hello Reflex!"


-- main = mainWidget $ el "div" $ do
--   x <- numberInput
--   let numberString = fmap (pack . show) x
--   dynText numberString
--
-- numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = do
--   n <- textInput $ def & textInputConfig_inputType .~ "number"
--                        & textInputConfig_initialValue .~ "0"
--   return . fmap (readMay . unpack) $ _textInput_value n

-- 错误的类型
-- main = mainWidget $ el "div" $ do
--   x <- numberInput
--   numberString <- mapDyn show x
--   dynText $ fmap Data.Text.pack numberString
--
-- numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = do
--   n <- textInput $ def & textInputConfig_inputType .~ "number"
--                        & textInputConfig_initialValue .~ "0"
--   mapDyn readMay $ _textInput_value n



-- main = mainWidget $ el "div" $ do
--   t <- textInput def
--   text "Last key pressed: "
--   let keypressEvent = fmap show $ _textInput_keypress t
--   keypressDyn <- holdDyn "None" keypressEvent
--   dynText $ fmap pack keypressDyn


-- main = mainWidget $ el "div" $ do
--     t <- textInput $ def & textInputConfig_inputType .~ "number"
--                          & textInputConfig_initialValue .~ "0"
--     dynText $ _textInput_value t
-- main = do
--   mainWidget $ el "div" $ do
--     tweetBox <- textArea def
--     dynText $ value tweetBox


-- main = do
--   mainWidget $ el "div" $ text "the text"

-- main = mainWidget $ el "div" $ do
--   el "hl" $ text "夕阳"
--   el "p"  $ text "文章"
--   tx <- textInput
--   dynText $ _textInput_value tx


-- main = mainWidget $ el "div" $ do
--   tx <- textInput dyn
--   dynText $ _textInput_value tx


-- main = mainWidget $ el "div" $ do
--   el "p" $ text "Reflex is:"
--   el "ul" $ do
--     el "li" $ text "Efficient"
--     el "li" $ text "Higher-order"
--     el "li" $ text "Glitch-free"
--     t <- textInput def
--     dynText $ _textInput_value t


-- main = mainWidget $ el "div" $ do
--   t <- textInput def
--   dynText $ _textInput_value t





  -- some :: (forall x. Widget x ()) -> IO ()
  -- some w = undefined


































{-- --

main :: IO ()
main = do
    mainWidget $ do
        header "Temperature Converter"

header :: MonadWidget t m => String -> m ()
header = el "h1" . text


--}
