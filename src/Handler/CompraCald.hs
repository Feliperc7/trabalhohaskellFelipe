{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Compracald where

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
                        \ INNER JOIN compracald on compracald.usuarioid = usuario.id \
                        \ INNER JOIN calda ON compracald.caldaid = calda.id \
                        \ WHERE usuario.id = ?"
                     caldas <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Compracald,Entity Calda)]
                     defaultLayout $ do
                        [whamlet|
                                 <body style="background-color:LightCyan;">
                             <center><caption> <h1> CALDAS COMPRADAS POR #{usuarioNome usuario}</caption><center><br>
                          <center><table width="60%" style="background-color:black; border:2px solid;text-align:center">

                                   <thead style="color: white">
                                       <th><h3>Calda</th>
                                       <th><h3>Preço Total</th>

                                   <tbody style="background-color: white">

                                     $forall (Entity _ _, Entity _ compracald, Entity _ calda) <- caldas
                                       <td> #{caldaNome calda}</td>
                                       <td> R$ #{caldaPreco calda * (fromIntegral (compracaldPote compracald))}</td><tr>
                                      
                        |]

postCompraCaldR :: CaldaId -> Handler Html
postCompraCaldR cid = do
    ((resp,_),_) <- runFormPost formQt
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
                               runDB $ insert (Compracald uid cid pote)
                               redirect ListCompraCaldR
         _ -> redirect HomeR
