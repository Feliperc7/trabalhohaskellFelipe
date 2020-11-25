{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Comprasorv where

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
                        \ INNER JOIN comprasorv ON comprasorv.usuarioid = usuario.id \
                        \ INNER JOIN sorvete ON comprasorv.sorveteid = sorvete.id \
                        \ WHERE usuario.id = ?"
                     sorvetes <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Comprasorv,Entity Sorvete)]
                     defaultLayout $ do
                        [whamlet|
                                <body style="background-color:SandyBrown;">
                             <center><caption> <h1> SORVETES COMPRADOS POR #{usuarioNome usuario}</caption><center><br>
                          <center><table width="60%" style="background-color:black; border:2px solid;text-align:center">

                                <thead style="color: white">
                                   <th><h3>Sorvete</th>
                                   <th><h3>Preço Total</th>

                                <tbody style="background-color: white">
                            
                                 $forall (Entity _ _, Entity _ comprasorv, Entity _ sorvete) <- sorvetes

                                  <td> #{sorveteNome sorvete}</td>
                                  <td> R$ #{sorvetePreco sorvete * (fromIntegral (comprasorvPote comprasorv))}</td><tr>
                                  <tr>

                                  
                        |]

postCompraSorvR :: SorveteId -> Handler Html
postCompraSorvR sid = do
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
                               runDB $ insert (Comprasorv uid sid pote)
                               redirect ListCompraSorvR
         _ -> redirect HomeR
