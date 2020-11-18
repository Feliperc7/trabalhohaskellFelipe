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
                                      (Just "hs3")
                                      Nothing
                                      [("class","classe3")]
                       ) (fmap acompanhamentoNome mp) 
    <*> areq doubleField "Preco: " (fmap acompanhamentoPreco mp) 

auxAcompR :: Route App -> Maybe Acompanhamento -> Handler Html    
auxAcompR rt mp = do
    (widget,_) <- generateFormPost (formAcompanhamento mp)
    defaultLayout [whamlet|
        <body style="background-color:LimeGreen;">
        <center><caption> <h1> CADASTRO DE ACOMPANHAMENTOS </caption></center><br>
        <center><form action=@{rt} method=post>
           ^{widget}
            <input type="submit" value="Cadastrar"></center>
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
          <body style="background-color:MediumSeaGreen;">
        
          <br><a href=@{HomeR}> 
          <input type="button" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold">

          <br><a href=@{AcompanhamentoR}> 
          <input type="button" value="CADASTRAR OUTRO ACOMPANHAMENTO" style="color:black;font-weight:bold"></a><br>

          <center><caption> <h1> ACOMPANHAMENTO CADASTRADO! </caption></center>
        
          <table width="100%" style="border:10px solid;text-align:center"> 
               <tr>
                <th><h2>Acompanhamento: <h2>#{acompanhamentoNome acompanhamento}
                <th><h2>Preço: <h2>R$ #{acompanhamentoPreco acompanhamento}
    |]

-- select * from Acompanhamento order by preco desc
getListaaR :: Handler Html 
getListaaR = do 
    -- acompanhamento :: [Entity AcompanhamentoId Acompanhamento]
    acompanhamentos <- runDB $ selectList [] [Desc AcompanhamentoPreco]
    defaultLayout [whamlet|
        <body style="background-color:lightgreen;">

        <br><a href=@{HomeR}> 
        <input type="button" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold">

        <br><a href=@{AcompanhamentoR}> 
        <input type="button" value="CADASTRAR OUTRO ACOMPANHAMENTO" style="color:black;font-weight:bold"></a><br>

        <center><table width="70%" style="background-color:forestgreen; border:2px solid;text-align:center">
       
         <caption> <h1> <center>ACOMPANHAMENTOS CADASTRADOS</center> </caption> 
            <thead style="color: white">  
                
                    <th><h1>Nome</th>
                    <th><h1>Preço</th>
             
            <tbody style="background-color: white">
                
                  $forall Entity aid acompanhamento <- acompanhamentos
                
                    <td><h3> #{acompanhamentoNome acompanhamento}</td>
                        
                    <td><h3> R$ #{acompanhamentoPreco acompanhamento}</td>
                            
                    <td><h3> <a href=@{UpdAcompR aid}> <input type="submit" value="Editar" style="color:blue"></a></td>

                    <td><h3> <form action=@{DelAcompR aid} method=post> <input type="submit" value="Excluir"style="color:red"></td><tr>

                <tr>
                <br>

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
    redirect ListaaR

