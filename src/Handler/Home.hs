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
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <body style="background-color:powderblue;">
            <h1>
                <center>SISTEMA DA SORVETERIA</center>
            
            <img src=@{StaticR img_sorvete_jpg}>
        
            <ul>
                <br><a href=@{SorveteR}">
                <input type="button" value="CADASTRO DE SORVETES" style="color:black;font-weight:bold"><br>              
                        
                <br><a href="@{ListasR}">
                <input type="button" value="LISTA DOS SORVETES" style="color:black;font-weight:bold"><br>
      
                <br><img src=@{StaticR img_calda_jpg}>

                <br><a href=@{CaldaR}">
                <input type="button" value="CADASTRO DE CALDAS" style="color:red;font-weight:bold"><br> 

                <br><a href=@{ListacR}">
                <input type="button" value="LISTA DAS CALDAS" style="color:red;font-weight:bold"><br> 
                   
                <br><img src=@{StaticR img_acompanhamento_jpg}>

                <br><a href=@{AcompanhamentoR}">
                <input type="button" value="CADASTRO DE ACOMPANHAMENTOS" style="blue:black;font-weight:bold"><br> 
                        
                <br><a href=@{ListaaR}">
                <input type="button" value="LISTA DOS ACOMPANHAMENTOS" style="blue:black;font-weight:bold"><br> 
                 
        |]