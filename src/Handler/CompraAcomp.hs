{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.CompraAcomp where

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
                        \ INNER JOIN compraAcomp on compraAcomp.usuarioid = usuario.id \
                        \ INNER JOIN acompanhamento ON compraAcomp.acompanhamentoid = acompanhamento.id \
                        \ WHERE usuario.id = ?"
                     acompanhamentos <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity CompraAcomp,Entity Acompanhamento)]
                     defaultLayout $ do
                        [whamlet|
                            <body style="background-color:LimeGreen;">
                            <h1>
                                 <center>ACOMPANHAMENTOS COMPRADOS POR #{usuarioNome usuario}</center>
                            <ul>
                                 $forall (Entity _ _, Entity _ compraAcomp, Entity _ acompanhamento) <- acompanhamentos
                                 
                                    <center>#{acompanhamentoNome acompanhamento}: #{acompanhamentoPreco acompanhamento *                                              (fromIntegral (compraAcompPote compraAcomp))}</center>
                        |] 


postCompraAcompR :: AcompanhamentoId -> Handler Html
postCompraAcompR aid = do
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
                               runDB $ insert (CompraAcomp uid aid pote)
                               redirect ListCompraAcompR
         _ -> redirect HomeR

