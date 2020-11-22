{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Text.Lucius

formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ (,)
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing
    
getEntrarR :: Handler Html
getEntrarR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $
        [whamlet|
            <body style="background-color:Aquamarine;">
            $maybe mensa <- msg
                <div>
               <center>^{mensa}
            <h1>
                <center>ENTRAR</center>
        
            <center><form method=post action=@{EntrarR}>
                ^{widget}
                <input type="submit" value="Entrar"></center>
        |]

postEntrarR :: Handler Html
postEntrarR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess ("root@root.com","root123") -> do
            setSession "_EMAIL" "root@root.com"
            redirect AdminR
        FormSuccess (email,senha) -> do
           -- select * from usuario where email=digitado.email
           usuario <- runDB $ getBy (UniqueEmail email)
           case usuario of
                Nothing -> do
                    setMessage [shamlet|
                        <div>
                            <center><h1 style="color:red;font-weight:bold">E-MAIL NÃO ENCONTRADO!</h1></center>
                    |]
                    redirect EntrarR
                Just (Entity _ usu) -> do
                    if (usuarioSenha usu == senha) then do
                        setSession "_EMAIL" (usuarioEmail usu)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            <div>
                               <center><h1 style="color:red;font-weight:bold">SENHA INCORRETA!</h1></center>
                        |]
                        redirect EntrarR
        _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
    deleteSession "_EMAIL"
    redirect HomeR

getAdminR :: Handler Html
getAdminR =
    defaultLayout [whamlet|
        <body style="background-color:BurlyWood;">
        <center><h1 style="color:green;font-weight:bold">BEM-VINDO ADMIN!!!</h1></center>
          
    |]