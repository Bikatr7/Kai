{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Yesod.Static

data HelloWorld = HelloWorld 
    { getStatic :: Static
    }

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/static StaticR Static getStatic
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "My Haskell Website"
    addStylesheet $ StaticR $ StaticRoute ["style.css"] []
    [whamlet|
        <div .container>
            <h1>Welcome to My Haskell Website
            <p>This is a simple website built with Yesod!
            <p>It's now working with proper static file serving.
    |]

main :: IO ()
main = do
    putStrLn "Starting server on http://localhost:3000"
    static <- static "static"
    warp 3000 $ HelloWorld static 