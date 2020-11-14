{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Acompanhamento where

import Import

formAcompanhamento :: Maybe Acompanhamento -> Form Acompanhamento
formAcompanhamento mp = renderDivs $ Acompanhamento
    <$> areq textField (FieldSettings "Nome: "
                                      Nothing
                                      (Just "hs1")
                                      Nothing
                                      [("class","classe1")]
                       ) (fmap acompanhamentoNome mp) 
    <*> areq doubleField "Preco: " (fmap acompanhamentoPreco mp) 

auxAcompR :: Route App -> Maybe Acompanhamento -> Handler Html    
auxAcompR rt mp = do
    (widget,_) <- generateFormPost (formAcompanhamento mp)
    defaultLayout [whamlet|
        <form action=@{rt} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar acompanhamento para o sorvete">
    |]

getAcompanhamentoR :: Handler Html
getAcompanhamentoR = auxAcompR AcompanhamentoR Nothing

postAcompanhamentoR :: Handler Html
postAcompanhamentoR = do
    ((res,_),_) <- runFormPost (formAcompanhamento Nothing)
    case res of 
         FormSuccess acompanhamento -> do
             aid <- runDB (insert acompanhamento)
             redirect (DescaR aid)
         _ -> redirect HomeR

-- SELECT * from Acompanhamento where id = aid
getDescaR :: AcompanhamentoId -> Handler Html
getDescaR aid = do
    acompanhamento <- runDB $ get404 aid
    defaultLayout [whamlet|
        <h1>
            Nome: #{acompanhamentoNome acompanhamento}
        
        <h2>
            Preço: #{acompanhamentoPreco acompanhamento}
    |]

-- select * from Acompanhamento order by preco desc
getListaaR :: Handler Html 
getListaaR = do 
    -- acompanhamento :: [Entity AcompanhamentoId Acompanhamento]
    acompanhamentos <- runDB $ selectList [] [Desc AcompanhamentoPreco]
    defaultLayout [whamlet|
        <table>
        <body style="background-color:lightgreen;">
            <thead>
                <tr>
                    <th>
                        Nome
                    <th>
                        Preco
                    <th>
                    
                    <th>
            
            <tbody>
                $forall Entity aid acompanhamento <- acompanhamentos
                    <tr>
                        <td>
                            #{acompanhamentoNome acompanhamento}
                        
                        <td>
                            #{acompanhamentoPreco acompanhamento}
                        
                        <td>
                            <a href=@{UpdAcompR aid}>
                                Editar
                        
                        <td>
                            <form action=@{DelAcompR aid} method=post>
                                <input type="submit" value="X">
    |]

getUpdAcompR :: AcompanhamentoId -> Handler Html
getUpdAcompR aid = do
    antigo <- runDB $ get404 aid 
    auxAcompR (UpdAcompR aid) (Just antigo)

-- UPDATE FROM acompanhamento WHERE id = aid SET ...
postUpdAcompR :: AcompanhamentoId -> Handler Html
postUpdAcompR aid = do
    ((res,_),_) <- runFormPost (formAcompanhamento Nothing)
    case res of 
         FormSuccess novo -> do
             _ <- runDB (replace aid novo)
             redirect ListaaR
         _ -> redirect HomeR

-- delete from acompanhamento where id = aid
postDelAcompR :: AcompanhamentoId -> Handler Html
postDelAcompR aid = do
    runDB $ delete aid 
    redirect ListacR

