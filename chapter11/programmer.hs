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
allOperatingSystems = [ GnuPlusLinux, OpenBSDPlusNeverMindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = map mkProgrammer [(x, y) | x <- allOperatingSystems, y <- allLanguages] where
  mkProgrammer :: (OperatingSystem, ProgLang) -> Programmer
  mkProgrammer (x, y) = Programmer x y