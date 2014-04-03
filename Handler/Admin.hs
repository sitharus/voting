module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
  regions <- runDB $ selectList ([] :: [Filter Region]) [] 
  (regionWidget, regionEnc) <- generateFormPost (regionForm Nothing)
  defaultLayout $ do
              setTitle "Admin"
              $(widgetFile "admin")

postRegionR :: Handler Html
postRegionR = do
  ((result, _), _) <- runFormPost (regionForm Nothing)
  _ <- case result of
    FormSuccess region -> do runDB $ insert region
    _ -> error "failed"
  redirect AdminR

getRegionOptionsR :: RegionId -> Handler Html
getRegionOptionsR regionId = do
  Just region <- runDB $ get regionId
  options <- runDB $ selectList [VoteOptionRegionId ==. regionId] []
  (widget, enctype) <- generateFormPost $ optionForm regionId Nothing
  defaultLayout $ do
    setTitle "Voting Options"
    $(widgetFile "regionOption")

postRegionOptionsR :: RegionId -> Handler Html
postRegionOptionsR regionId = do
    ((result, _), _) <- runFormPost $ optionForm regionId Nothing
    _ <- case result of
      FormSuccess option -> do runDB $ insert option
      _ -> error "Failed"
    redirect $ RegionOptionsR regionId

regionForm :: Maybe Region -> Html ->  MForm Handler (FormResult Region, Widget)
regionForm r = renderDivs $ Region <$> areq textField "Name" (regionName <$> r)


optionForm :: RegionId -> Maybe VoteOption -> Html -> MForm Handler (FormResult VoteOption, Widget)
optionForm regionId r = renderDivs $ 
               VoteOption regionId <$> areq textField "Name" (voteOptionName <$> r)
