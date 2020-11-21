{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius

formUsu :: Form (Usuario, Text)
formUsu = renderBootstrap $ (,)
    <$> (Usuario
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Confirme a senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,_) <- generateFormPost formUsu
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile  "templates/form.lucius")
        $(whamletFile "templates/form.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess (usuario,veri) -> do
            if (usuarioSenha usuario == veri) then do
                runDB $ insert usuario
                setMessage [shamlet|
                    <div>
                        
                        <h2 style="color:green;font-weight:bold">USUÁRIO INCLUIDO</h2>     
                |]
                redirect UsuarioR
            else do
                setMessage [shamlet|
                    <div>
                        
                        <h2 style="color:red;font-weight:bold">SENHA E VERIFICAÇÃO NÃO CONFEREM</h2>
                |]
                redirect UsuarioR
        _ -> redirect HomeR    


        