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
            <br><button onclick="<a href=@{SorveteR}>">CADASTRO DE SORVETES</button>
                
                        
            <br><a href="@{ListasR}">
                <input type="button" value="LISTA DOS SORVETES" style="color:black;font-weight:bold">
      

                <br><img src=@{StaticR img_calda_jpg}>

                <br><button onclick="<a href=@{CaldaR}>"><span style="color:black;font-                          weight:bold"></span></button>CADASTRO DE CALDAS<br>

                        
                <br><a href=@{ListacR}>
                         <br>LISTA DAS CALDAS<br>
                   
                <br><img src=@{StaticR img_acompanhamento_jpg}>

                <br><a href=@{AcompanhamentoR}>
                         CADASTRO DE ACOMPANHAMENTOS
                        
                <br><a href=@{ListaaR}>
                         LISTA DOS ACOMPANHAMENTOS
               
              
        |]