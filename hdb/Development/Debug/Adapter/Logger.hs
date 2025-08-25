{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple Logger API using co-log style loggers
module Development.Debug.Adapter.Logger (
  LogAction (..),
  Severity (..),
  WithSeverity (..),
  cmap,
  cmapIO,
  cmapWithSev,

  -- * Pretty printing of logs
  renderPrettyWithSeverity,
  renderWithSeverity,
  renderPretty,
  renderSeverity,
  renderWithTimestamp,
) where

import Control.Monad.IO.Class
import Control.Monad ((>=>))
import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Colog.Core.Action (cmap)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

cmapWithSev :: (a -> b) -> LogAction m (WithSeverity b) -> LogAction m (WithSeverity a)
cmapWithSev f = cmap (fmap f)

cmapIO :: MonadIO m => (a -> IO b) -> LogAction m b -> LogAction m a
cmapIO f LogAction{ unLogAction } =
  LogAction
    { unLogAction = (liftIO . f) >=> unLogAction }

renderPrettyWithSeverity :: Pretty a => WithSeverity a -> Text
renderPrettyWithSeverity =
  renderWithSeverity renderPretty

renderWithSeverity :: (a -> Text) -> WithSeverity a -> Text
renderWithSeverity f msgWithSev =
  renderSeverity (getSeverity msgWithSev) <> " " <> f (getMsg msgWithSev)

renderPretty :: Pretty a => a -> Text
renderPretty a =
  let
    docToText = renderStrict . layoutPretty defaultLayoutOptions
  in
    docToText (pretty a)

renderWithTimestamp :: Text -> IO Text
renderWithTimestamp msg = do
  t <- getCurrentTime
  let timeStamp = utcTimeToText t
  pure $ "[" <> timeStamp <> "] " <> msg
  where
    utcTimeToText utcTime = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" utcTime

renderSeverity :: Severity -> Text
renderSeverity = \ case
  Debug -> "[DEBUG]"
  Info -> "[INFO]"
  Warning -> "[WARNING]"
  Error -> "[ERROR]"
