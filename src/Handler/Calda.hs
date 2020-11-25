{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Calda where

import Import
import Tool

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
        <center><caption> <h1> CADASTRO DE CALDAS </caption></center><br>
        <center><form action=@{rt} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar"></center>
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
    (widget,_) <- generateFormPost formQt
    defaultLayout [whamlet|
        <body style="background-color:Turquoise;">

        <br><a href=@{HomeR}> 
        <input type="submit" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold">

        <br><a href=@{CaldaR}> 
        <input type="submit" value="CADASTRAR OUTRA CALDA" style="color:black;font-weight:bold"></a><br>

        <center><caption> <h1> CALDA CADASTRADA! </caption></center>
          
        <table width="100%" style="border:10px solid;text-align:center"> 
               <tr>
                <th><h2>Calda: <h2>#{caldaNome calda}
                <th><h2>Preço: <h2>R$ #{caldaPreco calda}

                <br><form action=@{CompraCaldR cid} method=post>
                ^{widget}
                    <input type="submit" value="Adicionar ao Carrinho">
           
        
    |]

getListacR :: Handler Html 
getListacR = do 
    -- caldas :: [Entity CaldaId Calda]
    caldas <- runDB $ selectList [] [Desc CaldaPreco]
    defaultLayout [whamlet|
        <body style="background-color:aquamarine;">
     
        <br><a href=@{HomeR}> 
        <input type="submit" value="VOLTAR PARA A PÁGINA INICIAL" style="color:black;font-weight:bold">

        <br><a href=@{CaldaR}> 
        <input type="submit" value="CADASTRAR OUTRA CALDA" style="color:black;font-weight:bold"></a><br>

        <center><table width="70%" style="background-color:maroon; border:2px solid;text-align:center">
       
         <caption> <h1> <center>CALDAS CADASTRADAS</center> </caption> 
            <thead style="color: white">  
                
                    <th><h1>Nome</th>
                    <th><h1>Preço</th>
             
            <tbody style="background-color: white">
                
                  $forall Entity cid calda <- caldas
                             
                    <td><h3> <a href=@{DesccR cid}> 
                             #{caldaNome calda}</a></td>
                        
                    <td><h3> R$ #{caldaPreco calda}</td>
                            
                    <td><h3> <a href=@{UpdCaldR cid}> <input type="submit" value="Editar" style="color:blue"></a></td>

                    <td><h3> <form action=@{DelCaldR cid} method=post> <input type="submit" value="Excluir"style="color:red"></td><tr>

                <tr>
                <br>

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
    runDB $ deleteCascade cid 
    redirect ListacR


