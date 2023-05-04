module Prompt
    (
    -- * User prompts
      PromptConfig(..)
    , promptYorN
    , promptChar
    , PromptVariant(..)
    , PromptVariantConfig(..)
    , promptChar'
    ) where


import Control.Monad.Trans ( liftIO )
import Data.Char ( toUpper, toLower )
import System.Console.Haskeline ( runInputT, defaultSettings, getInputChar, outputStrLn )

import Control.Monad (when, forM_)

data PromptConfig = PromptConfig { pPrompt :: String
                                 , pBasicCharacters :: [Char]
                                 , pAdvancedCharacters :: [Char] -- ^ only shown on help
                                 , pDefault :: Maybe Char
                                 , pHelp    :: [Char]
                                 }


-- | Prompt the user for a yes or no
promptYorN :: String -> IO Bool
promptYorN p = (== 'y') `fmap` promptChar (PromptConfig p "yn" [] Nothing [])


-- | Prompt the user for a character, among a list of possible ones.
--   Always returns a lowercase character. This is because the default
--   character (ie, the character shown in uppercase, that is automatically
--   selected when the user presses the space bar) is shown as uppercase,
--   hence users may want to enter it as uppercase.
promptChar :: PromptConfig -> IO Char
promptChar (PromptConfig p basic_chs adv_chs def_ch help_chs) =
  runInputT defaultSettings loopChar
 where
 chs = basic_chs ++ adv_chs
 loopChar = do
    let chars = setDefault (basic_chs ++ (if null adv_chs then "" else "..."))
        prompt = p ++ " [" ++ chars ++ "]" ++ helpStr
    a <- getInputChar prompt >>= maybe (liftIO $ fail "promptChar: unexpected end of input") (return . toLower)
    case () of
     _ | a `elem` chs                   -> return a
       | a == ' '                       -> maybe tryAgain return def_ch
       | a `elem` help_chs              -> return a
       | otherwise                      -> tryAgain
 helpStr = case help_chs of
           []                      -> ": "
           (h:_) | null adv_chs    -> ", or " ++ (h:" for help: ")
                 | otherwise       -> ", or " ++ (h:" for more options: ")
 tryAgain = do outputStrLn "Invalid response, try again!"
               loopChar
 setDefault s = case def_ch of Nothing -> s
                               Just d  -> map (setUpper d) s
 setUpper d c = if d == c then toUpper c else c


class Eq a => PromptVariant a where
  promptVariantChar :: a -> Char
  promptVariantHelp :: a -> String
  promptVariantIsAdvanced :: a -> Bool

data PromptVariantConfig a = PromptVariantConfig
    { promptVariantConfigPrompt :: String
    , promptVariantConfigItems :: [a]
    , promptVariantConfigDefault :: Maybe a
    }

promptChar' :: PromptVariant a => PromptVariantConfig a -> IO a
promptChar' p = do
  c <- promptChar makePromtConfig
  if c `elem` pHelp makePromtConfig
    then do
         runInputT defaultSettings showHelp
         promptChar' p
    else (return . head . filter (\x -> promptVariantChar x == c)) (promptVariantConfigItems p)
  where
    makePromtConfig = PromptConfig
      { pPrompt = promptVariantConfigPrompt p
      , pBasicCharacters = characters (not . promptVariantIsAdvanced)
      , pAdvancedCharacters = characters promptVariantIsAdvanced
      , pDefault = promptVariantChar <$> promptVariantConfigDefault p
      , pHelp = ['?', 'h']
      }
    characters predicat =
      (foldr (:) [] . fmap promptVariantChar . filter predicat) (promptVariantConfigItems p)
    showHelp = do
      outputStrLn "How to use:"
      printHelp $ not . promptVariantIsAdvanced
      outputStrLn ""
      printHelp promptVariantIsAdvanced
      outputStrLn ""
      outputStrLn "?: show this help"
      outputStrLn ""
      case promptVariantConfigDefault p of
        Nothing -> return ()
        Just i ->
          when (i `elem` promptVariantConfigItems p) $ do
            outputStrLn "<Space>: accept the current default (which is capitalized)"
            outputStrLn ""
      where
        printHelp predicat =
          forM_ (filter predicat (promptVariantConfigItems p)) $ \i ->
            outputStrLn ([promptVariantChar i] <> ": " <> promptVariantHelp i)
