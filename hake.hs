{-# LANGUAGE
    MultiWayIf
  , UnicodeSyntax
  #-}

import Hake

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫
    cabal ["clean"] ?> removeDirIfExists buildPath
                    >> cleanCabalLocal

  "build deps | install all the dependencies" ∫
    cabal ["install", "--only-dependencies", "--overwrite-policy=always"]

  bbExecutable ♯
   let processBuild =
           cabalConfigure
        >> cabalBuild
        >> getCabalBuildPath appName >>=
            \p -> copyFile p bbExecutable
    in processBuild ?> cleanCabalLocal

  "install | install to system" ◉ [bbExecutable] ∰
    cabal ["install", "--overwrite-policy=always"]

 where
  appName ∷ String
  appName = "bb"

  buildPath ∷ String
  buildPath = "dist-newstyle"

  bbExecutable ∷ String
  bbExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] -> buildPath </> appName ++ "exe"
       | otherwise                             -> buildPath </> appName
