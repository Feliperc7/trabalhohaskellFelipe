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
            <br><button onclick="a href=@{SorveteR}"><span style="color:black;font-weight:bold">CADASTRO DE SORVETES
                
                        
            <br><button onclick="a href=@{ListasR}"><span style="color:black;font-weight:bold">LISTA DOS SORVETES</span></button>
                    

                <br><img src=@{StaticR img_calda_jpg}>

                <br><a href=@{CaldaR}>
                         <br>CADASTRO DE CALDAS<br>
                        
                <br><a href=@{ListacR}>
                         <br>LISTA DAS CALDAS<br>
                   
                <br><img src=@{StaticR img_acompanhamento_jpg}>

                <br><a href=@{AcompanhamentoR}>
                         CADASTRO DE ACOMPANHAMENTOS
                        
                <br><a href=@{ListaaR}>
                         LISTA DOS ACOMPANHAMENTOS
               
              
        |]