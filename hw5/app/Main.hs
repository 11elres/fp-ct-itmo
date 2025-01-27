module Main
  ( main
  ) where

import           Control.Exception             (IOException, catch)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Set                      as Set
import           Prettyprinter                 (Pretty (pretty),
                                                defaultLayoutOptions,
                                                layoutPretty, line)
import           Prettyprinter.Render.Terminal (renderIO)
import           System.Console.Haskeline      (InputT, defaultSettings,
                                                getInputLine, runInputT)
import           System.IO                     (stdout)
import           Text.Megaparsec.Error         (errorBundlePretty)

import           HW5.Action                    (HiPermission (..), runHIO)
import           HW5.Base                      (HiExpr)
import           HW5.Evaluator                 (eval)
import           HW5.Parser                    (parse)
import           HW5.Pretty                    (prettyValue)

-- | REPL of hi language.
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      mInput <- getInputLine "hi> "
      case mInput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          liftIO $ either (liftIO . putStrLn . errorBundlePretty) evaluating (parse input)
          loop

    evaluating :: HiExpr -> IO ()
    evaluating expr = do
      let evalRes = (<> line) .
                      either 
                        (\e -> pretty $ "Evaluation error: " <> show e) 
                        prettyValue <$>
                      eval expr
      actionRes <- catch 
                    (runHIO evalRes $ Set.fromList [AllowRead ..])
                    (\e -> return $ pretty ("IO error: " <> show (e :: IOException)) <> line)
      renderIO stdout $ layoutPretty defaultLayoutOptions actionRes
