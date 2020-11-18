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
            <input type="submit" value="Cadastrar"></center>
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

getDescsR :: SorveteId -> Handler Html
getDescsR sid = do
    sorvete <- runDB $ get404 sid
    defaultLayout [whamlet|
          <body style="background-color:DarkSalmon;">

          <br><a href="@{HomeR}"> 
          <input type="button" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold">

          <br><a href="@{SorveteR}"> 
          <input type="button" value="CADASTRAR OUTRO SORVETE" style="color:black;font-weight:bold"></a><br>

          <center><caption> <h1> SORVETE CADASTRADO! </caption></center>

           <table width="100%" style="border:10px solid;text-align:center"> 
               <tr>
                <th><h1>Sorvete: <h2>#{sorveteNome sorvete}</th>
                <th><h1>Preço: <h2>R$ #{sorvetePreco sorvete}</th>
                       
    |]

   
getListasR :: Handler Html 
getListasR = do 
    sorvetes <- runDB $ selectList [] [Desc SorvetePreco]
    defaultLayout [whamlet|
        
        <body style="background-color:palegoldenrod">

        <br><a href=@{SorveteR}> 
        <input type="button" value="CADASTRAR OUTRO SORVETE" style="color:black;font-weight:bold"></a> 
           
        <br><a href=@{HomeR}> 
        <input type="button" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold"></a><br>
  
        <center><table width="70%" style="background-color:black; border:2px solid;text-align:center">
       
         <caption> <h1> <center>SORVETES CADASTRADOS</center> </caption> 
            <thead style="color: white">  
                
                    <th><h1>Nome</th>
                    <th><h1>Preço</th>
             
            <tbody style="background-color: white">
                
                  $forall Entity sid sorvete <- sorvetes
                
                    <td><h3> #{sorveteNome sorvete}</td>
                        
                    <td><h3> R$ #{sorvetePreco sorvete}</td>
                            
                    <td><h3> <a href=@{UpdSorvR sid}> <input type="submit" value="Editar" style="color:blue"></a></td>

                    <td><h3> <form action=@{DelSorvR sid} method=post> <input type="submit" value="Excluir"style="color:red"></td><tr>

                <tr>
                <br>
                
                           
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


