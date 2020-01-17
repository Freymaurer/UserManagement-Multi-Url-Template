module Client.View

open Elmish
open Elmish.UrlParser
open Elmish.Navigation
open Thoth.Json
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fable.FontAwesome
open Fulma

open Shared

open Client.AuxFunctions
open Client.View.Helper
open Client.Types
open Client.State

let addUsernameToExtLoginModal model dispatch =
    Modal.modal [
        Modal.IsActive true
        Modal.Props  [
            if model.RegisterModel.Username = "" then
                yield (onEnter (AddUsernameToExtLogin model.RegisterModel.Username) dispatch)
            else yield Style []
        ]
    ] [
        Modal.background [ Props [OnClick (fun _ -> dispatch (UpdateExtraElement EmptyElement) )] ] [ ]
        Modal.Card.card [
            Modifiers [ Modifier.BackgroundColor IsWhite ]
            Props [ Style [ Height "80%";BorderRadius "15px" ] ]
        ] [
            Modal.Card.head [
                Modifiers [Modifier.BackgroundColor IsWhite]
                Props [ Style [ BorderBottom "0px"] ]
                ] [
                Modal.Card.title [ Props [ Style [ PaddingTop "2rem" ] ] ] [
                    str "Create your Username"
                ]
            ]
            Modal.Card.body  [ ] [
                text [][
                    str "Thank you for using our external login service!"
                    str "Please enter your preferred username. It will be used to show your activities and will hold correlation to your login method."
                ]
                Box.box' [ Props [ Class "registerBox" ] ] [
                    Text.div [ Props [ Style [ PaddingLeft "1rem" ] ] ] [
                        str "Create your Username"
                    ]
                    Input.text [
                        Input.OnChange (fun e ->
                            let newModel = {model.RegisterModel with Username = e.Value}
                            dispatch (UpdateRegisterModel newModel)
                            )
                        Input.Props [
                            Style [
                                BoxShadow "none"; Border "none";
                                BackgroundColor "#f2f2f2"
                            ]
                        ]
                    ]
                ]
                Columns.columns [ Columns.Props [ Style [ PaddingTop "2rem" ] ] ] [
                    Column.column [Column.Offset (Screen.All,Column.IsHalf)] [
                        Button.button [
                            Button.Color IsInfo
                            (if model.RegisterModel.Username = "" then Button.Disabled true else Button.Disabled false )
                            Button.OnClick (fun _ -> dispatch (AddUsernameToExtLogin model.RegisterModel.Username))
                        ][
                            str "Register Username"
                        ]
                    ]
                ]
            ]
        ]
    ]

let registerModal (model : Model) (dispatch : Msg -> unit) =
    Modal.modal [
        Modal.IsActive true
        Modal.Props [onEnter (DotnetRegisterRequest model.RegisterModel) dispatch]
    ] [
        Modal.background [ Props [OnClick (fun _ -> dispatch (UpdateExtraElement EmptyElement) )] ] [ ]
        Modal.Card.card [
            Modifiers [ Modifier.BackgroundColor IsWhite ]
            Props [ Style [ Height "80%";BorderRadius "15px" ] ]
        ] [
            Modal.Card.head [
                Modifiers [Modifier.BackgroundColor IsWhite]
                Props [ Style [ BorderBottom "0px"] ]
                ] [
                Modal.Card.title [ Props [ Style [ PaddingTop "2rem" ] ] ] [
                    str "Create your account"
                ]
            ]
            Modal.Card.body  [ ] [
                inputRegisterBox
                    []
                    Input.text
                    "Username"
                    model.RegisterModel.Username
                    (fun e ->
                        let newModel = {model.RegisterModel with Username = e.Value}
                        dispatch (UpdateRegisterModel newModel)
                        )
                inputRegisterBox
                    []
                    Input.email
                    "Email"
                    model.RegisterModel.Email
                    (fun e ->
                        let newModel = {model.RegisterModel with Email = e.Value}
                        dispatch (UpdateRegisterModel newModel)
                        )
                inputRegisterBox
                    []
                    Input.password
                    "Password"
                    model.RegisterModel.Password
                    (fun e ->
                        let newModel = {model.RegisterModel with Password = e.Value}
                        dispatch (UpdateRegisterModel newModel)
                        )
                Columns.columns [ Columns.Props [ Style [ PaddingTop "2rem" ] ] ] [
                    Column.column [Column.Offset (Screen.All,Column.IsThreeQuarters)] [
                        Button.button [
                            Button.Color IsInfo
                            (if model.RegisterModel.Username = "" || model.RegisterModel.Password = "" || model.RegisterModel.Email = "" then Button.Disabled true else Button.Disabled false )
                            Button.OnClick (fun _ -> dispatch (DotnetRegisterRequest model.RegisterModel))
                        ][
                            str "Register"
                        ]
                    ]
                ]
            ]
        ]
    ]

module VerifyLoginModalInputElements =

    ///needs empty model because it will be given to modal functions that always wants a (model -> (Msg -> unit) -> ReactElement) function
    let private inputUsername model dispatch =
        Container.container [][
            text [][str "Type in your new username."]
            Column.column [][
                Input.text [Input.OnChange (fun e -> dispatch (UpdateInputString e.Value)); Input.Placeholder "... New Username"]
            ]
        ]

    let private inputEmail model dispatch =
        Container.container [][
            text [][str "Type in your new Email adress."]
            Column.column [][
                Input.email [Input.OnChange (fun e -> dispatch (UpdateInputString e.Value)); Input.Placeholder "... New Email"]
            ]
        ]

    let private inputPw model dispatch =
        Container.container [][
            text [][str "Type in your new password."]
            Column.column [][
                Input.password [Input.OnChange (fun e -> dispatch (UpdateInputString e.Value)); Input.Placeholder "... New Password"]
            ]
        ]

    let private inputUsernameAdmin model dispatch =
        Container.container [][
            text [][str "You are about to change a users account parameters! Type in the new username."]
            Column.column [][
                Input.text [Input.OnChange (fun e -> dispatch (UpdateInputString e.Value)); Input.Placeholder "... New Username"]
            ]
        ]

    let private inputEmailAdmin model dispatch =
        Container.container [][
            text [][str "You are about to change a users account parameters! Type in the new Email adress."]
            Column.column [][
                Input.email [Input.OnChange (fun e -> dispatch (UpdateInputString e.Value)); Input.Placeholder "... New Email"]
            ]
        ]

    let private inputPwAdmin model dispatch =
        Container.container [][
            text [][str "You are about to change a users account parameters! Type in the new password."]
            Column.column [][
                Input.password [Input.OnChange (fun e -> dispatch (UpdateInputString e.Value)); Input.Placeholder "... New Password"]
            ]
        ]

    let private inputRoleAdmin (model:Model) dispatch =
        Container.container [][
            text [][str "You are about to change a users account parameters! Type in the new role."]
            Column.column [][
                Dropdown.dropdown [ Dropdown.IsHoverable;Dropdown.IsUp] [
                    div [ ] [
                        Button.button [ Button.Modifiers [Modifier.BackgroundColor IsWhiteTer] ] [
                            span [ ] [ str (if model.InputString = "" then "Role" else model.InputString)]
                            Icon.icon [ Icon.Size IsSmall ] [ Fa.i [ Fa.Solid.AngleDown ] [ ] ]
                        ]
                    ]
                    Dropdown.menu [ ] [
                        Dropdown.content [ ] [
                            Dropdown.Item.a [ Dropdown.Item.Props [OnClick (fun _ -> dispatch (UpdateInputString (string ActiveUserRoles.Admin))) ] ] [ str "Admin" ]
                            Dropdown.Item.a [ Dropdown.Item.Props [OnClick (fun _ -> dispatch (UpdateInputString (string ActiveUserRoles.UserManager))) ] ] [ str "UserManager" ]
                            Dropdown.divider [ ]
                            Dropdown.Item.a [ Dropdown.Item.Props [OnClick (fun _ -> dispatch (UpdateInputString (string ActiveUserRoles.User))) ] ] [ str "User" ]
                        ]
                    ]
                ]
            ]
        ]

    open Client.AuxFunctions.UserAccountAux

    let parseUserAccountInputEle (inputEle: UserAccountAux.InputSubElements) =
        let strReactElement stringVal model dispatch =
            str stringVal
        match (inputEle:UserAccountAux.InputSubElements) with
        | InputUsername -> inputUsername
        | InputEmail -> inputEmail
        | InputPw -> inputPw
        | InputUsernameAdmin -> inputUsernameAdmin
        | InputEmailAdmin -> inputEmailAdmin
        | InputPwAdmin -> inputPwAdmin
        | InputRoleAdmin -> inputRoleAdmin
        | DeleteAccountText -> strReactElement "Delete your account"
        | AdminDeleteAccountText -> strReactElement "You are about to delete a user account. Please verify your login."

let verifyLoginModal (model : Model) (extraElement: (Model -> (Msg -> unit) -> ReactElement)) (dispatch : Msg -> unit) msgInput =
    let extUser = model.User.Value.ExtLogin.IsTrue
    let msg =
        match msgInput with
        | DeleteAccountRequest _ -> DeleteAccountRequest model.LoginModel
        | AdminDeleteAccountRequest _ -> AdminDeleteAccountRequest (model.LoginModel,model.AdminViewUser.Value)
        | DotnetChangeUserParamRequest (_,userParam,_) -> DotnetChangeUserParamRequest (model.LoginModel,userParam,model.InputString)
        | AdminChangeUserParamsRequest (_,_,userParam,_) -> AdminChangeUserParamsRequest (model.LoginModel,model.AdminViewUser.Value,userParam,model.InputString)
        | _ -> UpdateExtraElement (Message "There went something wrong! This should never happen")
    Modal.modal [
        Modal.IsActive true
        Modal.Props [onEnter msg dispatch]
    ] [
        Modal.background [ Props [OnClick (fun _ -> dispatch (UpdateExtraElement EmptyElement) )] ] [ ]
        Modal.Card.card [
            Modifiers [ Modifier.BackgroundColor IsWhite ]
            Props [ Style [ Height "80%";BorderRadius "15px" ] ]
        ] [
            Modal.Card.head [
                Modifiers [Modifier.BackgroundColor IsWhite]
                Props [ Style [ BorderBottom "0px"] ]
            ] [
                Modal.Card.title [ Props [ Style [ Padding "0.75em 1em";BorderBottom "1px solid grey" ] ] ] [
                    str "Verify your Login"
                ]
            ]
            Modal.Card.body  [ ] [
                div [][
                    text [][str "Verify your login information. This is done to increase security."]
                    inputRegisterBox
                        []
                        Input.text
                        "Current Username"
                        model.LoginModel.Username
                        (fun e -> dispatch (UpdateLoginUsername e.Value))
                    inputRegisterBox
                        [Hidden extUser]
                        Input.password
                        "Current Password"
                        model.LoginModel.Password
                        (fun e -> dispatch (UpdateLoginUserPw e.Value))
                ]
                extraElement model dispatch
                Columns.columns [ Columns.Props [ Style [ PaddingTop "2rem" ] ] ] [
                    Column.column [Column.Offset (Screen.All,Column.IsHalf)] [
                        Button.button [
                            Button.Color IsDanger
                            ( if model.LoginModel.Username <> "" && (model.LoginModel.Password <> "" || extUser) then Button.Disabled false else Button.Disabled true )
                            Button.OnClick (fun _ -> dispatch msg)
                        ][ str "Verify" ]
                    ]
                ]
            ]
        ]
    ]

let adminRegisterModal model dispatch =
    let adminView =
        Column.column [][
            Dropdown.dropdown [ Dropdown.IsHoverable;Dropdown.IsUp] [
                div [ ] [
                    Button.button [ Button.Modifiers [Modifier.BackgroundColor IsWhiteTer] ] [
                        span [ ] [ str (if model.AdminAssignRole = Guest || model.AdminAssignRole = NoRole then "Role" else string model.AdminAssignRole) ]
                        Icon.icon [ Icon.Size IsSmall ] [ Fa.i [ Fa.Solid.AngleDown ] [ ] ]
                    ]
                ]
                Dropdown.menu [ ] [
                    Dropdown.content [ ] [
                        Dropdown.Item.a [ Dropdown.Item.Props [OnClick (fun _ -> dispatch (AdminSelectAssignRole ActiveUserRoles.Admin)) ] ] [ str "Admin" ]
                        Dropdown.Item.a [ Dropdown.Item.Props [OnClick (fun _ -> dispatch (AdminSelectAssignRole ActiveUserRoles.UserManager)) ] ] [ str "UserManager" ]
                        Dropdown.divider [ ]
                        Dropdown.Item.a [ Dropdown.Item.Props [OnClick (fun _ -> dispatch (AdminSelectAssignRole ActiveUserRoles.User)) ] ] [ str "User" ]
                    ]
                ]
            ]
        ]
    Modal.modal [
        Modal.IsActive true
        Modal.Props [onEnter (AdminRegisterUserRequest (model.RegisterModel,model.AdminAssignRole)) dispatch]
    ] [
        Modal.background [ Props [OnClick (fun _ -> dispatch (UpdateExtraElement EmptyElement) )] ] [ ]
        Modal.Card.card [
            Modifiers [ Modifier.BackgroundColor IsWhite ]
            Props [ Style [ Height "80%";BorderRadius "15px" ] ]
        ] [
            Modal.Card.head [
                Modifiers [Modifier.BackgroundColor IsWhite]
                Props [ Style [ BorderBottom "0px"] ]
            ] [
                Modal.Card.title [ Props [ Style [ PaddingTop "2rem" ] ] ] [
                    str "Create your account"
                ]
            ]
            Modal.Card.body[ ] [
                inputRegisterBox
                    []
                    Input.text
                    "Username"
                    model.RegisterModel.Username
                    (fun e ->
                        let newModel = {model.RegisterModel with Username = e.Value}
                        dispatch (UpdateRegisterModel newModel)
                        )
                inputRegisterBox
                    []
                    Input.email
                    "Email"
                    model.RegisterModel.Email
                    (fun e ->
                        let newModel = {model.RegisterModel with Email = e.Value}
                        dispatch (UpdateRegisterModel newModel)
                        )
                inputRegisterBox
                    []
                    Input.password
                    "Password"
                    model.RegisterModel.Password
                    (fun e ->
                        let newModel = {model.RegisterModel with Password = e.Value}
                        dispatch (UpdateRegisterModel newModel)
                        )
                Columns.columns [ Columns.Props [ Style [ PaddingTop "2rem" ] ] ] [
                    adminView
                    Column.column [] [
                        Button.button [
                            Button.Color IsInfo
                            (if model.RegisterModel.Username = "" || model.RegisterModel.Password = "" || model.RegisterModel.Email = "" || model.AdminAssignRole = Guest || model.AdminAssignRole = NoRole then Button.Disabled true else Button.Disabled false )
                            Button.OnClick (fun _ -> dispatch (AdminRegisterUserRequest (model.RegisterModel,model.AdminAssignRole)))
                        ][
                            str "Register"
                        ]
                    ]
                ]
            ]
        ]
    ]

open VerifyLoginModalInputElements
open Client.AuxFunctions.UserAccountAux
open Client.AuxFunctions.UserListAux

let extraEle model dispatch =
    match model.ExtraReactElement with
    | EmptyElement -> emptyStr
    | RegisterModal -> registerModal model dispatch
    | AdminRegisterModal -> adminRegisterModal model dispatch
    | Message x -> messageContainer x (fun _ -> dispatch (UpdateExtraElement EmptyElement))
    | VerifyLoginModal (x,ele) -> verifyLoginModal model ele dispatch x
    | AddUsernameToExternLoginModal -> addUsernameToExtLoginModal model dispatch
    | UserAccountExtraElements ele ->
        match ele with
        | VerifyChangeUserParamsEle (userParam, str, inputEle) ->
            let msg = DotnetChangeUserParamRequest (model.LoginModel,userParam, str)
            let ele = parseUserAccountInputEle inputEle
            verifyLoginModal model ele dispatch msg
        | VerifyAdminChangeUserParamsEle (targetUser, userParam, str, inputEle) ->
            let msg = AdminChangeUserParamsRequest (model.LoginModel, model.AdminViewUser.Value, userParam, str)
            let ele = parseUserAccountInputEle inputEle
            verifyLoginModal model ele dispatch msg
        | DeleteAccountRequestEle (inputEle) ->
            let msg = Msg.DeleteAccountRequest model.LoginModel
            let ele = parseUserAccountInputEle inputEle
            verifyLoginModal model ele dispatch msg
        | AdminDeleteAccountRequestEle (targetUser, inputEle) ->
            let msg = Msg.AdminDeleteAccountRequest (model.LoginModel, model.AdminViewUser.Value)
            let ele = parseUserAccountInputEle inputEle
            verifyLoginModal model ele dispatch msg
    | UserListExtraElements ele ->
        match ele with
        | AdminRegisterModalEle -> adminRegisterModal model dispatch

        //| _ -> failwith "not yet done" // done, noice