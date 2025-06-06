{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                      as T
import           GI.Gtk                         ( Widget
                                                , toWidget
                                                , widgetShowAll
                                                )
import           System.Taffybar
import           System.Taffybar.Context        ( TaffybarConfig(..)
                                                , TaffyIO
                                                )
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU2
import           System.Taffybar.Information.Memory
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util           ( runCommandFromPath )
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util

main :: IO ()
main = dyreTaffybar . appendHook notifySystemD $ myConfig

transparent, yellow1, yellow2, green1, green2, taffyBlue
  :: (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig :: GraphConfig
myGraphConfig = defaultGraphConfig { graphPadding         = 0
                                   , graphBorderWidth     = 0
                                   , graphWidth           = 75
                                   , graphBackgroundColor = transparent
                                   }

netCfg :: GraphConfig
netCfg = myGraphConfig { graphDataColors = [yellow1, yellow2]
                       , graphLabel      = Just "net"
                       }

memCfg :: GraphConfig
memCfg = myGraphConfig { graphDataColors = [taffyBlue]
                       , graphLabel      = Just "mem"
                       }

cpuCfg :: GraphConfig
cpuCfg = myGraphConfig { graphDataColors = [green1, green2]
                       , graphLabel      = Just "cpu"
                       }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = getCPULoad "cpu"

notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]

myConfig :: TaffybarConfig
myConfig =
  let myWorkspacesConfig = defaultWorkspacesConfig
        { minIcons        = 1
        , widgetGap       = 0
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      bat        = textBatteryNew "$percentage$%"
      cpu        = pollingGraphNew cpuCfg 1 cpuCallback
      mem        = pollingGraphNew memCfg 1 memCallback
      net        = networkGraphNew netCfg Nothing
      netmon     = networkMonitorNew defaultNetFormat Nothing
      clock      = textClockNewWith defaultClockConfig
        { clockUpdateStrategy = ConstantInterval 1.0
        , clockFormatString   = "%a %b %d %H:%M"
        }
      layout     = layoutNew defaultLayoutConfig
      windowsW   = windowsNew defaultWindowsConfig
      tray       = sniTrayNew
      myConfig   = defaultSimpleTaffyConfig
        { startWidgets  = workspaces : map (>>= buildContentsBox) [layout, windowsW]
        , endWidgets    = map (>>= buildContentsBox)
                            [bat, batteryIconNew, volumeNew, clock, tray, cpu, mem, netmon, net]
                            -- [bat, batteryIconNew, volumeNew, clock, tray, cpu, mem, netmon, net, mpris2New]
        , barPosition   = Bottom
        , barPadding    = 10
        , barHeight     = 50
        , widgetSpacing = 1
        }
  in  withBatteryRefresh . withLogServer . withToggleServer . toTaffyConfig $ myConfig

------------- Volume status ---------------

volumeNew :: TaffyIO Widget
volumeNew = do
  label <- pollingLabelNew 1 tryGetVolume
  widgetShowAll label
  toWidget label

data AudioStatus = AudioOn | AudioOff deriving (Eq, Show)

-- could use the alsa-mixer package but that requires a fork of taffybar to add the package; not today.
parseVolume :: IO (Int, AudioStatus)
parseVolume = do
  runCommandFromPath ["amixer", "get", "Master"] >>= \case
    Left _  -> return (0, AudioOff)
    Right s ->
      let raw    = takeWhile (/= '\n') $ dropWhile (/= '[') s
          volume = read $ takeWhile (/= '%') $ drop 1 (takeWhile (/= ' ') raw)
          status = case takeWhile (/= ']') $ drop 2 (dropWhile (/= ' ') raw) of
                     "on" -> AudioOn
                     _    -> AudioOff
      in return (volume, status)

volIcon :: Int -> Text
volIcon x | x == 0 = "🔇"
          | x < 30 = "🔈"
          | x < 60 = "🔉"
          | True   = "🔊"

tryGetVolume :: IO Text
tryGetVolume = (try getVolume :: IO (Either SomeException Text)) <&> \case
  Left _  -> "Error"
  Right x -> x

getVolume :: IO Text
getVolume = parseVolume <&> \case
  (_, AudioOff) -> volIcon 0 <> " Mute"
  (v, AudioOn)  -> volIcon v <> T.pack (" " <> show v <> "%")

-----------------------------------------
