module Options
  ( Options(..)
  , defaultOptions
  , getOptions
  )
where


import Options.Applicative


data Options = Options
  { _optionNumberOfRounds       :: Int
  , _optionInitialNumberOfSteps :: Int
  }

defaultOptions :: Options
defaultOptions = Options
  { _optionNumberOfRounds       = 3
  , _optionInitialNumberOfSteps = 3
  }

optionParser :: Parser Options
optionParser =
  Options
    <$> option auto
        (long "nb-rounds"
        <> short 'r'
        <> help "Number of rounds"
        <> showDefault
        <> value (_optionNumberOfRounds defaultOptions)
        <> metavar "INT")
    <*> option auto
        (long "nb-initial-steps"
        <> short 's'
        <> help "Number of initial steps"
        <> showDefault
        <> value (_optionInitialNumberOfSteps defaultOptions)
        <> metavar "INT")

optionParserInfo :: ParserInfo Options
optionParserInfo =
  info
    (optionParser <**> helper)
    (fullDesc
     <> progDesc "Generate a circuit training from your terminal"
     <> header "Major Michiara's circuit training" )

getOptions :: IO Options
getOptions = execParser optionParserInfo 