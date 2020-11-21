{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.CompraSorv where

import Import
import Tool
import Database.Persist.Sql

getListCompraSorvR :: Handler Html
getListCompraSorvR = do
    sess <- lookupSession"_EMAIL"
    case sess of
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of
                 Nothing -> redirect HomeR
                 Just (Entity uid usuario) -> do
                     let sql = "SELECT ??,??,?? FROM usuario \
                        \ INNER JOIN compraSorv ON compraSorv.usuarioid = usuario.id \
                        \ INNER JOIN sorvete ON compraSorv.sorveteid = sorvete.id \
                        \ WHERE usuario.id = ?"
                     sorvetes <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity CompraSorv,Entity Sorvete)]
                     defaultLayout $ do
                        [whamlet|
                            <h1>
                                 SORVETES COMPRADOS POR #{usuarioNome usuario}
                            <ul>
                                 $forall (Entity _ _, Entity _ compraSorv, Entity _ sorvete) <- sorvetes
                                     
                                         #{sorveteNome sorvete}: #{sorvetePreco sorvete * (fromIntegral (compraSorvPote compraSorv))}
                        
                        |]

postCompraSorvR :: SorveteId -> Handler Html
postCompraSorvR sid = do
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
                               runDB $ insert (CompraSorv uid sid pote)
                               redirect ListCompraSorvR
         _ -> redirect HomeR

