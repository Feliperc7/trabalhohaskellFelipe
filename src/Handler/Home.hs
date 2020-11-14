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
            <h1>
                SISTEMA DE PRODUTOS
        
            <img src=@{StaticR imgs_sorvete_jpg}>
        
            <ul>
                <li>
                    <a href=@{SorveteR}>
                         <br>CADASTRO DE SORVETES<br>
                        
                <li>
                    <a href=@{ListasR}>
                         <br>LISTA DOS SORVETES<br>

                <li>

                    <img src=@{StaticR imgs_calda_jpg}>

                    <a href=@{CaldaR}>
                         <br>CADASTRO DE CALDAS<br>
                        
                <li>
                    <a href=@{ListacR}>
                         <br>LISTA DAS CALDAS<br>

                <li>

                    <img src=@{StaticR imgs_acompanhamento_jpg}>

                    <a href=@{AcompanhamentoR}>
                         <br>CADASTRO DE ACOMPANHAMENTOS<br>
                        
                <li>
                    <a href=@{ListaaR}>
                         <br>LISTA DOS ACOMPANHAMENTOS<br>

        |]