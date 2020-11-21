{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    sess <- lookupSession "_EMAIL"
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <body style="background-color:powderblue;">
            
                <center><h1>SISTEMA DA SORVETERIA</center>
                
                <marquee><h2>Seja bem-vindo ao sistema da sorveteria - Aqui você pode cadastrar, listar, editar                              e remover sabores de sorvete, caldas e acompanhamentos!!!<br></marquee>

                <br><a href="@{UsuarioR}">
                <input type="button" value="CADASTRO DE USUÁRIO" style="color:blue;font-weight:bold"></a>

                $maybe email <- sess
                        <div>
                            <center>SEJA BEM-VINDO, #{email}</center>
                            <form action=@{SairR} method=post>
                                <input type="submit" value="SAIR" style="color:red;font-weight:bold">
                $nothing
                    
                <br><a href="@{EntrarR}">
                <input type="button" value="ENTRAR" style="color:blue;font-weight:bold"></a><br>

                <br><img src=@{StaticR img_sorvete_jpg}> 
            
                <br><a href="@{SorveteR}">                                                                
                <input type="button" value="CADASTRO DE SORVETES" style="color:black;font-weight:bold"> 
                        
                <br><a href="@{ListasR}">                                                                 
                <input type="button" value="LISTA DOS SORVETES" style="color:black;font-weight:bold"></a><br> 
      
                <br><img src=@{StaticR img_calda_jpg}>

                <br><a href="@{CaldaR}">
                <input type="button" value="CADASTRO DE CALDAS" style="color:red;font-weight:bold">

                <br><a href="@{ListacR}">
                <input type="button" value="LISTA DAS CALDAS" style="color:red;font-weight:bold"></a><br> 
                   
                <br><img src=@{StaticR img_acompanhamento_jpg}>

                <br><a href="@{AcompanhamentoR}">
                <input type="button" value="CADASTRO DE ACOMPANHAMENTOS" style="color:green;font-weight:bold"> 
                        
                <br><a href="@{ListaaR}">
                <input type="button" value="LISTA DOS ACOMPANHAMENTOS" style="color:green;font-weight:bold"><br> 
                 
        |]