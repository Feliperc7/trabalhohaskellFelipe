{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Sorvete where

import Import

formSorvete :: Maybe Sorvete -> Form Sorvete
formSorvete mp = renderDivs $ Sorvete
    <$> areq textField (FieldSettings "Nome: "
                                      Nothing
                                      (Just "hs1")
                                      Nothing
                                      [("class","classe1")]
                       ) (fmap sorveteNome mp) 
    <*> areq doubleField "Preco: " (fmap sorvetePreco mp) 

auxSorveteR :: Route App -> Maybe Sorvete -> Handler Html    
auxSorveteR rt mp = do
    (widget,_) <- generateFormPost (formSorvete mp)
    defaultLayout [whamlet|
        <form action=@{rt} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar sabor de sorvete">
    |]

getSorveteR :: Handler Html
getSorveteR = auxSorveteR SorveteR Nothing

postSorveteR :: Handler Html
postSorveteR = do
    ((res,_),_) <- runFormPost (formSorvete Nothing)
    case res of 
         FormSuccess sorvete -> do
             sid <- runDB (insert sorvete)
             redirect (DescsR sid)
         _ -> redirect HomeR

-- SELECT * from sorvete where id = sid
getDescsR :: SorveteId -> Handler Html
getDescsR sid = do
    sorvete <- runDB $ get404 sid
    defaultLayout [whamlet|
        <h1>
            Nome: #{sorveteNome sorvete}
        
        <h2>
            Preço: #{sorvetePreco sorvete}
    |]

-- select * from sorvete order by preco desc
getListasR :: Handler Html 
getListasR = do 
    -- sorvetes :: [Entity ProdutoId Produto]
    sorvetes <- runDB $ selectList [] [Desc SorvetePreco]
    defaultLayout [whamlet|
        <table>
            <thead>
                <tr>
                    <th>
                        Nome
                    <th>
                        Preco
                    <th>
                    
                    <th>
            
            <tbody>
                $forall Entity sid sorvete <- sorvetes
                    <tr>
                        <td>
                            #{sorveteNome sorvete}
                        
                        <td>
                            #{sorvetePreco sorvete}
                        
                        <td>
                            <a href=@{UpdSorvR sid}>
                                Editar
                        
                        <td>
                            <form action=@{DelSorvR sid} method=post>
                                <input type="submit" value="X">
    |]

getUpdSorvR :: SorveteId -> Handler Html
getUpdSorvR sid = do
    antigo <- runDB $ get404 sid 
    auxSorveteR (UpdSorvR sid) (Just antigo)

-- UPDATE FROM sorvete WHERE id = sid SET ...
postUpdSorvR :: SorveteId -> Handler Html
postUpdSorvR sid = do
    ((res,_),_) <- runFormPost (formSorvete Nothing)
    case res of 
         FormSuccess novo -> do
             _ <- runDB (replace sid novo)
             redirect ListasR
         _ -> redirect HomeR

-- delete from sorvete where id = sid
postDelSorvR :: SorveteId -> Handler Html
postDelSorvR sid = do
    runDB $ delete sid 
    redirect ListasR


