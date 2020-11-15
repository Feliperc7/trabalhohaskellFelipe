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
        <body style="background-color:DarkSalmon;">
        <center><caption> <h1> CADASTRO DE SORVETES </caption></center><br>
        <center><form action=@{rt} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar sorvete"></center>
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
          <body style="background-color:DarkSalmon;">
          <center><caption> <h1> SORVETE CADASTRADO! </caption></center>

           <table width="100%" style="margin: 0 auto; border:3px solid;text-align:left"> 
                <th><h1>Sorvete: <h2>#{sorveteNome sorvete}
                <th><h1>Preço: <h2>R$ #{sorvetePreco sorvete}
          

                 <br><a href="@{HomeR}"> 
                 <input type="button" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold">

                 <br><a href="@{SorveteR}"> 
                 <input type="button" value="CADASTRAR OUTRO SORVETE" style="color:black;font-weight:bold"> 
        
    |]
     

getListasR :: Handler Html 
getListasR = do 
    -- sorvetes :: [Entity SorveteId Sorvete]
    sorvetes <- runDB $ selectList [] [Desc SorvetePreco]
    defaultLayout [whamlet|
        <table border="3">
        <body style="background-color:palegoldenrod;">
        <caption> <h1> <center>SORVETES CADASTRADOS</center> </caption>
                <thead>
                  <tr>
                     <th>Nome</th>
                     <th>Preço</th>
                <tbody>
                $forall Entity sid sorvete <- sorvetes
                  <tr>
                     <td> #{sorveteNome sorvete} </td>
                        
                     <td> R$ #{sorvetePreco sorvete} </td> 
                        
                     <td> <a href=@{UpdSorvR sid}> 
                                Editar             </td>
     
                     <td> <form action=@{DelSorvR sid} method=post>
                          <input type="submit" value="Excluir" style="color:red"> </td>

                     <br><a href="@{SorveteR}"> 
                     <input type="button" value="CADASTRAR OUTRO SORVETE" style="color:black;font-weight:bold"> 
           
                     <br><a href="@{HomeR}"> 
                     <input type="button" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold">
               
    |]

getUpdSorvR :: SorveteId -> Handler Html
getUpdSorvR sid = do
    antigo <- runDB $ get404 sid 
    auxSorveteR (UpdSorvR sid) (Just antigo)

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


