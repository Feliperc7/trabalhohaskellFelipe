{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Calda where

import Import

formCalda :: Maybe Calda -> Form Calda
formCalda mp = renderDivs $ Calda
    <$> areq textField (FieldSettings "Nome: "
                                      Nothing
                                      (Just "hs2")
                                      Nothing
                                      [("class","classe2")]
                       ) (fmap caldaNome mp) 
    <*> areq doubleField "Preco: " (fmap caldaPreco mp) 

auxCaldaR :: Route App -> Maybe Calda -> Handler Html    
auxCaldaR rt mp = do
    (widget,_) <- generateFormPost (formCalda mp)
    defaultLayout [whamlet|
        <body style="background-color:CadetBlue;">
        <center><form action=@{rt} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar calda"></center>
    |]

getCaldaR :: Handler Html
getCaldaR = auxCaldaR CaldaR Nothing

postCaldaR :: Handler Html
postCaldaR = do
    ((res,_),_) <- runFormPost (formCalda Nothing)
    case res of 
         FormSuccess calda -> do
             cid <- runDB (insert calda)
             redirect (DesccR cid)
         _ -> redirect HomeR

-- SELECT * from Calda where id = cid
getDesccR :: CaldaId -> Handler Html
getDesccR cid = do
    calda <- runDB $ get404 cid
    defaultLayout [whamlet|
        <body style="background-coor:Turquoise;">
        <caption> <h1> <center>CADASTRO DE CALDAS</center> </caption>
        <h1>
            Nome: #{caldaNome calda}
        
        <h2>
            Preço: #{caldaPreco calda}
    |]

-- select * from Calda order by preco desc
getListacR :: Handler Html 
getListacR = do 
    -- caldas :: [Entity CaldaId Calda]
    caldas <- runDB $ selectList [] [Desc CaldaPreco]
    defaultLayout [whamlet|
        <table>
        <body style="background-color:aquamarine;">
        <caption> <h1> <center>CALDAS CADASTRADAS</center> </caption>
            <thead>
                <tr>
                    <th>
                        Nome
                    <th>
                        Preco
                    <th>
                    
                    <th>
            
            <tbody>
                $forall Entity cid calda <- caldas
                    <tr>
                        <td>
                            #{caldaNome calda}
                        
                        <td>
                            #{caldaPreco calda}
                        
                        <td>
                            <a href=@{UpdCaldR cid}>
                                Editar
                        
                        <td>
                            <form action=@{DelCaldR cid} method=post>
                                <input type="submit" value="X">
    |]

getUpdCaldR :: CaldaId -> Handler Html
getUpdCaldR cid = do
    antigo <- runDB $ get404 cid 
    auxCaldaR (UpdCaldR cid) (Just antigo)

-- UPDATE FROM calda WHERE id = cid SET ...
postUpdCaldR :: CaldaId -> Handler Html
postUpdCaldR cid = do
    ((res,_),_) <- runFormPost (formCalda Nothing)
    case res of 
         FormSuccess novo -> do
             _ <- runDB (replace cid novo)
             redirect ListacR
         _ -> redirect HomeR

-- delete from calda where id = cid
postDelCaldR :: CaldaId -> Handler Html
postDelCaldR cid = do
    runDB $ delete cid 
    redirect ListacR


