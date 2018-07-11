data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNeverMindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNeverMindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {lang = x , os = y} | x <- allLanguages, y <- allOperatingSystems]
