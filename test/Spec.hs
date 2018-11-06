{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude

import qualified API
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Config as Config
import qualified Data.Set as Set
import qualified Data.User as User
import qualified Database
import qualified Server
import qualified System.Exit as Exit
import qualified System.Process as Process
import qualified Test.QuickCheck as QuickCheck

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Config (Config)
import Data.String (fromString)
import Test.Hspec (Spec, before, describe, hspec, it, runIO)
import Test.Hspec.Wai (ResponseMatcher, get, shouldRespondWith)


main :: IO ()
main = bracket setup teardown (hspec . spec)


spec :: Database.Connection 'Database.Open -> Spec
spec connection' = do
    connection <- runIO initializeDatabase
    users      <- runIO (populateDatabase connection)

    let stuff :: API.Stuff
        stuff = API.Stuff config connection

    before (pure (Server.application stuff)) $ do
        describe "GET /users/count" $ do
            it "responds with 200" $ do
                get "/users/count" `shouldRespondWith` 200

            it "responds with the correct users count" $ do
                let correctCount :: ResponseMatcher
                    correctCount = fromString (show (length users))
                get "/users/count" `shouldRespondWith` correctCount
  where
    initializeDatabase :: IO (Database.Connection 'Database.Initialized)
    initializeDatabase = runDB $ Database.initialize connection'

    populateDatabase
        :: Database.Connection 'Database.Initialized -> IO [Database.User]
    populateDatabase connection = do
        userNames <- Set.fromList <$> QuickCheck.generate QuickCheck.arbitrary
        traverse (createUser connection) (Set.toList userNames)

    createUser
        :: Database.Connection 'Database.Initialized
        -> User.Name
        -> IO Database.User
    createUser connection userName = runDB $ do
        Database.checkUserNameAvailability userName connection >>= \case
            Nothing                -> error "this shouldnt' happen"
            Just availableUserName -> do
                user <- Database.createNewUser availableUserName connection
                pure user


setup :: IO (Database.Connection 'Database.Open)
setup = do
    createdb testDatabaseName >>= checkExitCode "createdb failed"

    connection <- runExceptT (Database.connect databaseConnectionInfo)
        >>= checkError "error opening database connection"

    pure connection


teardown :: Database.Connection 'Database.Open -> IO ()
teardown connection = do
    _ <- Database.close connection
    dropdb testDatabaseName >>= checkExitCode "dropdb failed"
    pure ()


config :: Config
config = Config.new 8080 Config.Development databaseConnectionInfo


databaseConnectionInfo :: Database.ConnectionInfo
databaseConnectionInfo = Database.ConnectionInfo
    { Database.connHost     = "localhost"
    , Database.connPort     = Database.defaultPort
    , Database.connUser     = "tester"
    , Database.connPassword = "tester"
    , Database.connDatabase = "servant-example-test"
    }


testDatabaseName :: String
testDatabaseName =
    ByteString.unpack (Database.connDatabase databaseConnectionInfo)


createdb :: String -> IO Exit.ExitCode
createdb dbname = runProcess (Process.proc "createdb" [dbname])


dropdb :: String -> IO Exit.ExitCode
dropdb dbname = runProcess (Process.proc "dropdb" [dbname])


runProcess :: Process.CreateProcess -> IO Exit.ExitCode
runProcess process = do
    (_, _, _, processHandle) <- Process.createProcess process
    Process.waitForProcess processHandle


checkExitCode :: String -> Exit.ExitCode -> IO ()
checkExitCode errorContext = \case
    Exit.ExitSuccess -> pure ()
    Exit.ExitFailure code ->
        Exit.die
            $         show code
            `because` "non-zero exit code"
            `because` errorContext


checkError :: Exception e => String -> Either e a -> IO a
checkError errorContext = \case
    Left  err -> Exit.die (show err `because` errorContext)
    Right a   -> pure a


runDB :: Exception e => ExceptT e IO a -> IO a
runDB = runExceptT >=> either throwIO pure


addContext :: String -> String -> String
addContext ctx str = ctx <> ": " <> str


because :: String -> String -> String
because = flip addContext
