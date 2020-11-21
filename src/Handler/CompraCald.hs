{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.CompraCald where

import Import
import Tool
import Database.Persist.Sql

getListCompraCaldR :: Handler Html
getListCompraCaldR = do
    sess <- lookupSession"_EMAIL"
    case sess of
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of
                 Nothing -> redirect HomeR
                 Just (Entity uid usuario) -> do
                     let sql="SELECT ??,??,??FROM usuario \
                        \ INNER JOIN compraCald on compraCald.usuarioid = usuario.id \
                        \ INNER JOIN calda ON compraCald.caldaid = calda.id \
                        \ WHERE usuario.id = ?"
                     caldas <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity CompraCald,Entity Calda)]
                     defaultLayout $ do
                        [whamlet|
                            <body style="background-color:steelblue;">
                            <h1>
                                 <center>CALDAS COMPRADAS POR #{usuarioNome usuario}</center>
                            <ul>
                                 $forall (Entity _ _, Entity _ compraCald, Entity _ calda) <- caldas
                                 
                                     <center>#{caldaNome calda}: #{caldaPreco calda * (fromIntegral (compraCaldPote                                              compraCald))}</center>
                        |]

postCompraCaldR :: CaldaId -> Handler Html
postCompraCaldR cid = do
    ((resp,_),_) <- runFormPost formPote
    case resp of
         FormSuccess pote -> do
             sess <- lookupSession "_EMAIL"
             case sess of
                  Nothing -> redirect HomeR
                  Just email -> do
                      usuario <- runDB $ getBy (UniqueEmail email)
                      case usuario of
                           Nothing -> redirect HomeR
                           Just (Entity uid _) -> do
                               runDB $ insert (CompraCald uid cid pote)
                               redirect ListCompraCaldR
         _ -> redirect HomeR

