import Graphics.XOSD
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

main :: IO ()
main = do threadDelay tenminutes
          time <- getCurrentTime
          let time' = formatTime defaultTimeLocale "%R" time
          runXOSD [ Timeout 2
                   , VAlign VAlignTop
                   , HAlign HAlignRight
                   , Font "-*-dejavu sans mono-medium-r-*-*-17-*-*-*-*-*-*-*"
                   , Color "LimeGreen"
                   , Display (String time')] 
                   (const $ return ())
          main
           where second = 1000
                 minute = second * 60
                 tenminutes = 10 * minute
                