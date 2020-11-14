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
            <br><button onclick="a href=@{SorveteR}"><span style="color:black;font-weight:bold">CADASTRO DE SORVETES</span></button>
                <input type="submit">
                        
                <button onclick="a href=@{ListasR}"><span style="color:black;font-weight:bold">LISTA DOS SORVETES</span></button>
                    

                <b<img src=@{StaticR img_calda_jpg}>

                <li><a href=@{CaldaR}>
                         <br>CADASTRO DE CALDAS<br>
                        
                <li><a href=@{ListacR}>
                         <br>LISTA DAS CALDAS<br>
                   
                    <img src=@{StaticR img_acompanhamento_jpg}>

                <li><a href=@{AcompanhamentoR}>
                         <br>CADASTRO DE ACOMPANHAMENTOS<br>
                        
                <li><a href=@{ListaaR}>
                         <br>LISTA DOS ACOMPANHAMENTOS<br>
               
                ul li {
                         display: inline-block;
                      }

        |]