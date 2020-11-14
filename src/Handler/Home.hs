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
                SIISTEMA DA SORVETERIA
        
            <img src=@{StaticR img_sorvete_jpg}>
        
            <ul>
                <li><a href=@{SorveteR}>
                         <br>CADASTRO DE SORVETES<br>
                        
                <li><a href=@{ListasR}>
                         <br>LISTA DOS SORVETES<br>

                    <img src=@{StaticR img_calda_jpg}>

                <li><a href=@{CaldaR}>
                         <br>CADASTRO DE CALDAS<br>
                        
                <li><a href=@{ListacR}>
                         <br>LISTA DAS CALDAS<br>
                   
                    <img src=@{StaticR img_acompanhamento_jpg}>

                <li><a href=@{AcompanhamentoR}>
                         <br>CADASTRO DE ACOMPANHAMENTOS<br>
                        
                <li><a href=@{ListaaR}>
                         <br>LISTA DOS ACOMPANHAMENTOS<br>

        |]