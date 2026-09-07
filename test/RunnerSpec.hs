module RunnerSpec where

import Control.Monad (forM_)
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec
import ExampleSpec (withTempDir)
import SiteExportSpec (writeExecutable)

spec :: Spec
spec = describe "Kai runner and installation" $ do
  forM_ [0,7] $ \exitStatus ->
    it ("launches a shebang script directly with arguments and exit status " ++ show exitStatus) $ withTempDir $ \directory -> do
      found <- findExecutable "kai"
      binary <- case found of
        Just path -> canonicalizePath path
        Nothing -> expectationFailure "Built Kai executable missing from PATH" >> pure ""
      let bin = directory </> "bin"
          script = directory </> "executable script.kai"
      createDirectory bin
      createFileLink binary (bin </> "kai")
      writeExecutable script $ unlines
        [ "#!/usr/bin/env kai"
        , "// expect: error ExitRequested " ++ show exitStatus
        , "print (head args); print (head (tail args)); exit " ++ show exitStatus
        , "print \"must not execute\""
        ]
      inherited <- getEnvironment
      let environment = ("PATH", bin ++ ":" ++ systemPath) : filter ((/= "PATH") . fst) inherited
          process = (proc script ["argument with spaces", "--version"]) {cwd = Just directory, env = Just environment}
          expectedCode = if exitStatus == 0 then ExitSuccess else ExitFailure exitStatus
      timeout 5000000 (readCreateProcessWithExitCode process "")
        `shouldReturn` Just (expectedCode, "argument with spaces\n--version\n", "")

  it "honors KAI_BIN without kai on PATH, preserving arguments and exit status" $
    withRunner $ \root runner -> do
      let binary = root </> "explicit binary"
      writeExecutable binary "#!/bin/bash\nprintf '<%s>\\n' \"$@\"\nexit 7\n"
      result <- runRunner root runner ["hello world", "", "$(literal)", "--version"] [("KAI_BIN",binary)]
      result `shouldBe` (ExitFailure 7,"<hello world>\n<>\n<$(literal)>\n<--version>\n","")

  forM_ ["missing", "directory", "non-executable", "self", "copy", "legacy"] $ \kind ->
    it ("rejects an invalid explicit override: " ++ kind) $
      withRunner $ \root runner -> do
        let candidate = root </> "override"
        case kind of
          "directory" -> createDirectory candidate
          "non-executable" -> writeFile candidate "#!/bin/bash\nexit 0\n"
          "self" -> createFileLink runner candidate
          "copy" -> copyFile runner candidate
          "legacy" -> copyFile "test/fixtures/legacy-kai-runner.sh" candidate
          _ -> pure ()
        (code,out,err) <- runRunner root runner [] [("KAI_BIN",candidate)]
        code `shouldBe` ExitFailure 1
        out `shouldBe` ""
        err `shouldContain` "KAI_BIN must name an executable other than the Kai runner"

  it "searches beyond itself and duplicate runners on PATH" $
    withRunner $ \root runner -> do
      let first = root </> "first"
          second = root </> "second"
          final = root </> "final"
      mapM_ (createDirectoryIfMissing True) [first,second,final]
      copyFile runner (first </> "kai")
      copyFile runner (second </> "kai")
      probe (final </> "kai") "real"
      result <- runRunner root runner [] [("PATH",first ++ ":" ++ second ++ ":" ++ final ++ ":" ++ systemPath)]
      result `shouldBe` (ExitSuccess,"real\n","")

  it "skips an older installed runner without looping or losing the checkout" $
    withRunner $ \root runner -> do
      let oldBin = root </> "old bin"
          linkedBin = root </> "linked bin"
      mapM_ (createDirectoryIfMissing True) [oldBin,linkedBin]
      copyFile "test/fixtures/legacy-kai-runner.sh" (oldBin </> "kai")
      createFileLink runner (linkedBin </> "kai")
      localBuild root ".stack-work/dist/arch/ghc/build/kai/kai" "current"
      result <- runRunner root (linkedBin </> "kai") []
        [("PATH",linkedBin ++ ":" ++ oldBin ++ ":" ++ systemPath)]
      result `shouldBe` (ExitSuccess,"current\n","")

  it "handles relative invocation, relative PATH entries, and empty PATH entries" $
    withRunner $ \root runner -> do
      copyFile runner (root </> "kai")
      createDirectory (root </> "real bin")
      probe (root </> "real bin/kai") "relative"
      result <- runRunner root "./kai" [] [("PATH",":.:real bin:" ++ systemPath)]
      result `shouldBe` (ExitSuccess,"relative\n","")

  it "prefers a real PATH executable over repository builds" $
    withRunner $ \root runner -> do
      localBuild root ".stack-work/dist/arch/ghc/build/kai/kai" "local"
      createDirectory (root </> "bin")
      probe (root </> "bin/kai") "path"
      result <- runRunner root runner [] [("PATH",root </> "bin" ++ ":" ++ systemPath)]
      result `shouldBe` (ExitSuccess,"path\n","")

  it "uses the newest build without Stack, including the GHC dist layout" $
    withRunner $ \root runner -> do
      let old = ".stack-work/install/arch/aaa/ghc/bin/kai"
      localBuild root old "old"
      (code,_,_) <- readProcessWithExitCode "touch" ["-t","200001010000",root </> "repo" </> old] ""
      code `shouldBe` ExitSuccess
      localBuild root ".stack-work/dist/arch/ghc/build/kai/kai" "new"
      result <- runRunner root runner [] []
      result `shouldBe` (ExitSuccess,"new\n","")

  it "uses Stack's active snapshot even when another cache is newer" $
    withRunner $ \root runner -> do
      let active = root </> "active"
      createDirectoryIfMissing True (active </> "bin")
      probe (active </> "bin/kai") "active"
      localBuild root ".stack-work/install/arch/aaa/ghc/bin/kai" "wrong"
      mockStack root
      result <- runRunner root runner [] [("PATH",root </> "tools" ++ ":" ++ systemPath),("KAI_TEST_INSTALL",active)]
      result `shouldBe` (ExitSuccess,"active\n","")

  forM_ ["missing-build", "path-command-failure"] $ \mode ->
    it ("falls back to Stack with the repository configuration: " ++ mode) $
      withRunner $ \root runner -> do
        localBuild root ".stack-work/install/arch/aaa/ghc/bin/kai" "stale"
        mockStack root
        result <- runRunner root runner ["-e","print 42"]
          [("PATH",root </> "tools" ++ ":" ++ systemPath),("KAI_TEST_INSTALL",root </> "missing"),("KAI_TEST_MODE",mode)]
        result `shouldBe` (ExitSuccess,
          unlines ["--stack-yaml",root </> "repo/stack.yaml","exec","kai","--","-e","print 42"],"")

  it "resolves a relative symlink chain from another directory" $
    withRunner $ \root runner -> do
      localBuild root ".stack-work/dist/arch/ghc/build/kai/kai" "linked"
      createDirectory (root </> "bin")
      createFileLink "../repo/scripts/kai" (root </> "bin/inner")
      createFileLink "inner" (root </> "bin/kai")
      result <- runRunner root (root </> "bin/kai") [] [("PATH",root </> "bin" ++ ":" ++ systemPath)]
      result `shouldBe` (ExitSuccess,"linked\n","")

  it "reports missing builds without looping through duplicate runners" $
    withRunner $ \root runner -> do
      createDirectory (root </> "bin")
      copyFile runner (root </> "bin/kai")
      (code,out,err) <- runRunner root runner [] [("PATH",root </> "bin" ++ ":" ++ systemPath)]
      code `shouldBe` ExitFailure 1
      out `shouldBe` ""
      err `shouldContain` "kai: no runnable binary found"

  it "lets a standalone copied runner use Stack in the caller's project" $
    withRunner $ \root runner -> do
      createDirectory (root </> "standalone")
      let copied = root </> "standalone/kai"
      copyFile runner copied
      mockStack root
      result <- runRunner root copied ["--version"] [("PATH",root </> "tools" ++ ":" ++ systemPath)]
      result `shouldBe` (ExitSuccess,"exec\nkai\n--\n--version\n","")

  it "installs, reinstalls, runs outside the repository, and uninstalls with a spaced prefix" $
    withRunner $ \root runner -> do
      localBuild root ".stack-work/dist/arch/ghc/build/kai/kai" "installed"
      let prefix = root </> "install prefix"
          installed = prefix </> "bin/kai"
          makeArgs target = ["-s","-C",root </> "repo","PREFIX=" ++ prefix,target]
      forM_ [1 :: Int,2] $ \_ -> do
        (code,_,err) <- readProcessWithExitCode "make" (makeArgs "install") ""
        code `shouldBe` ExitSuccess
        err `shouldBe` ""
      pathIsSymbolicLink installed `shouldReturn` True
      result <- runRunner root installed [] [("PATH",prefix </> "bin" ++ ":" ++ systemPath)]
      result `shouldBe` (ExitSuccess,"installed\n","")
      (code,_,err) <- readProcessWithExitCode "make" (makeArgs "uninstall") ""
      code `shouldBe` ExitSuccess
      err `shouldBe` ""
      doesPathExist installed `shouldReturn` False
      doesFileExist runner `shouldReturn` True

systemPath :: String
systemPath = "/usr/bin:/bin"

withRunner :: (FilePath -> FilePath -> IO a) -> IO a
withRunner action = withTempDir $ \root -> do
  let repo = root </> "repo"
      runner = repo </> "scripts/kai"
  createDirectoryIfMissing True (takeDirectory runner)
  copyFile "scripts/kai" runner
  copyFile "Makefile" (repo </> "Makefile")
  writeFile (repo </> "stack.yaml") ""
  writeFile (repo </> "package.yaml") ""
  action root runner

probe :: FilePath -> String -> IO ()
probe path label = writeExecutable path ("#!/bin/bash\nprintf '%s\\n' '" ++ label ++ "'\n")

localBuild :: FilePath -> FilePath -> String -> IO ()
localBuild root relative label = do
  let binary = root </> "repo" </> relative
  createDirectoryIfMissing True (takeDirectory binary)
  probe binary label

mockStack :: FilePath -> IO ()
mockStack root = do
  createDirectoryIfMissing True (root </> "tools")
  writeExecutable (root </> "tools/stack") $ unlines
    [ "#!/bin/bash"
    , "if [[ ${3:-} == path && ${4:-} == --local-install-root ]]; then"
    , "  if [[ ${KAI_TEST_MODE:-} == path-command-failure ]]; then exit 1; fi"
    , "  printf '%s\\n' \"${KAI_TEST_INSTALL}\""
    , "else"
    , "  printf '%s\\n' \"$@\""
    , "fi"
    ]

runRunner :: FilePath -> FilePath -> [String] -> [(String,String)] -> IO (ExitCode,String,String)
runRunner directory runner arguments overrides = do
  inherited <- getEnvironment
  let settings = overrides ++ [("PATH",systemPath),("KAI_BIN","")]
      environment = foldr (\pair rest -> pair : filter ((/= fst pair) . fst) rest) inherited settings
      process = (proc "/bin/bash" (runner:arguments)) {cwd=Just directory,env=Just environment}
  result <- timeout 5000000 (readCreateProcessWithExitCode process "")
  case result of
    Just output -> pure output
    Nothing -> expectationFailure "Runner did not terminate within five seconds" >> pure (ExitFailure 124,"","")
