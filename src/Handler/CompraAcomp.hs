{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Compraacomp where

import Import
import Tool
import Database.Persist.Sql

getListCompraAcompR :: Handler Html
getListCompraAcompR = do
    sess <- lookupSession"_EMAIL"
    case sess of
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of
                 Nothing -> redirect HomeR
                 Just (Entity uid usuario) -> do
                     let sql = "SELECT ??,??,??FROM usuario \
                        \ INNER JOIN compraacomp on compraacomp.usuarioid = usuario.id \
                        \ INNER JOIN acompanhamento ON compraacomp.acompanhamentoid = acompanhamento.id \
                        \ WHERE usuario.id = ?"
                     acompanhamentos <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Compraacomp,Entity Acompanhamento)]
                     defaultLayout $ do
                        [whamlet|
                          <body style="background-color:PaleGoldenrod;">
                          <center><caption> <h1> ACOMPANHAMENTOS COMPRADOS POR #{usuarioNome usuario}</caption><center><br>
                          <center><table width="60%" style="background-color:black; border:2px solid;text-align:center">

                                <thead style="color: white">
                                   <th><h2>Acompanhamento</th>
                                   <th><h2>Preço Total</th>

                                <tbody style="background-color: white">
                            
                                 $forall (Entity _ _, Entity _ compraacomp, Entity _ acompanhamento) <- acompanhamentos
                                  <td> #{acompanhamentoNome acompanhamento}</td>
                                  <td> R$ #{acompanhamentoPreco acompanhamento * (fromIntegral (compraacompPote                                             compraacomp))}</td><tr>
                                  

                        |] 


postCompraAcompR :: AcompanhamentoId -> Handler Html
postCompraAcompR aid = do
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
                               runDB $ insert (Compraacomp uid aid pote)
                               redirect ListCompraAcompR
         _ -> redirect HomeR



