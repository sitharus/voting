{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth (maybeAuthId)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  authId <- maybeAuthId
  case authId of
    Just user -> votingPage user
    Nothing -> 
        defaultLayout $ do
                setTitle "Welcome to Voting!"
                $(widgetFile "homepage")

regionOptions r =
        selectList [VoteOptionRegionId ==. r] []

votingForm :: VoterId -> Voter -> Widget
votingForm voterId voter = do
  voterRegion <- return $ voterVoterRegion voter
  case voterRegion of
    Just v -> do
      allOptions <- handlerToWidget $ runDB $ selectList [VoteOptionRegionId ==. v] []
      votes <- handlerToWidget $ runDB $ selectList [VoteVoterId ==. voterId] []
      $(widgetFile "voteForm")
    Nothing -> [whamlet|Set your region above|]

votingPage :: VoterId -> Handler Html
votingPage v = do
  Just voter <- runDB $ get v
  (widget, enctype) <- generateFormPost $ regionSelector voter
  options <- case voterVoterRegion voter of
               Just region -> runDB $ regionOptions region
               Nothing -> return $ []
  votingForm <- return $ votingForm v voter
  defaultLayout $ do
                 setTitle "Cast your vote!"
                 $(widgetFile "voting")

postSaveRegionR :: Handler Html
postSaveRegionR = do
  Just authId <- maybeAuthId
  Just voter <- runDB $ get authId
  ((result, widget), enctype) <- runFormPost $ regionSelector voter
  case result of
    FormSuccess regionId -> runDB $ update authId [VoterVoterRegion =. Just regionId]
    _ -> error "Form failed"
  redirect HomeR

regionSelector :: Voter -> Form RegionId
regionSelector r = 
    let options = selectField $ optionsPersistKey ([] :: [Filter Region]) [] regionName
        field = areq options "" (voterVoterRegion r)
    in renderDivs field 
