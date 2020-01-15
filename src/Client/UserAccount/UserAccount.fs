module Client.UserAccount

open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fulma
open Elmish

open Shared
open Client.AuxFunctions
open Client.View.Helper

type Model = {
    ErrorMsg : string option
    User : User option
    Loading : bool
    Authenticated : bool
    ExtraReactElement : ExtraReactElement<Msg,Model>
    AdminViewUser : User option
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
and Msg =
/// needs role and userId as input
| DotnetGetUserByIdRequest of string * string
| DotnetGetUserByIdResponse of Result<string*DotnetViewUserResults,exn>
| UpdateExtraElement of ExtraReactElement<Msg,Model>
// Server Msg
| InitialUserLoaded of User
/// not necessary to define in "UserAccount.fs", as this will not be transmitted to the UserAccount.update function.
/// But instead will be used in States.update directly!
| UpdateMainExtraElement of UserAccountAux.ExtraReactElements
| NavigateUserList

module Server =

    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let userApi : IUserApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder ServerPath.normalizeRoutes
        |> Remoting.buildProxy<IUserApi>

    let dotnetSecureApi : IDotnetSecureApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder ServerPath.normalizeRoutes
        |> Remoting.buildProxy<IDotnetSecureApi>

    let dotnetAdminSecureApi: IAdminSecureApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder ServerPath.normalizeRoutes
        |> Remoting.buildProxy<IAdminSecureApi>


// defines the initial state and initial command (= side-effect) of the application
let init (role,id) : Model * Cmd<Msg> =
    let initialModel = {
        ErrorMsg = None
        User = None
        Loading = true
        Authenticated = false
        ExtraReactElement = EmptyElement
        AdminViewUser = None
    }
    let getUserCmd =
        Cmd.OfAsync.either
            Server.dotnetSecureApi.dotnetGetUserById
            (role,id)
            (Ok >> DotnetGetUserByIdResponse)
            (Error >> DotnetGetUserByIdResponse)
    initialModel, getUserCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
        ///
    | InitialUserLoaded initialUser ->
        let nextModel =
            if initialUser.ExtLogin.IsTrue && initialUser.ExtLogin.IsUsernameSet = false
            then
                { currentModel with User = Some initialUser; Loading = false;Authenticated = false }
            else
                { currentModel with User = Some initialUser; Loading = false;Authenticated = true }
        nextModel, Cmd.none
        ///
    | UpdateExtraElement (element) ->
        let nextModel = { currentModel with ExtraReactElement = element }
        nextModel,Cmd.none
        ///
    | DotnetGetUserByIdResponse (Ok (role,userResult)) ->
        let m, cmd =
            match userResult with
            | DotnetViewUserResults.ViewUserSuccess (user) ->
                match role with
                | "user" -> { currentModel with User = Some user; Authenticated = true }, Cmd.none
                | "admin" ->
                    let logInCmd =
                        Cmd.OfAsync.perform
                            Server.dotnetSecureApi.dotnetGetUser
                            ()
                            InitialUserLoaded
                    { currentModel with AdminViewUser = Some user }, logInCmd
                | _ -> {currentModel with ErrorMsg = Some "unknown role in url path"}, Cmd.none
            | DotnetViewUserResults.ViewUserFail (e) ->
                {currentModel with ErrorMsg = Some e}, Cmd.none
        m, cmd
        ///
    | DotnetGetUserByIdResponse (Error e) ->
        let nextModel = {
            currentModel with
                ErrorMsg = Some e.Message
        }
        nextModel, Cmd.none

///////////////////////////////////////////////////////////////////
///         View for User Account Information      ////////////////
///////////////////////////////////////////////////////////////////

let userAccountinformationColumn headerStr informationStr msg =
    Columns.columns [][
        Column.column [][
            Heading.h5 [][str headerStr]
            Heading.h6 [Heading.IsSubtitle][str informationStr]
        ]
        Column.column [ Column.Modifiers [Modifier.TextAlignment (Screen.All,TextAlignment.Right)] ][
            Button.a [ Button.OnClick msg ][str "Change"]
        ]
    ]

let guestAccountInformationColumn headerStr informationStr =
    Columns.columns [][
        Column.column [][
            Heading.h5 [][str headerStr]
            Heading.h6 [Heading.IsSubtitle][str informationStr]
        ]
    ]

open Client.AuxFunctions.UserAccountAux

//check for admin or user and give back different modals when changing account parameters
let extraElementUserName model =
    if model.AdminViewUser.IsNone
    then VerifyChangeUserParamsEle (Username, "", InputUsername)
    else VerifyAdminChangeUserParamsEle (model.AdminViewUser.Value, Username, "", InputUsernameAdmin)
let extraElementEmail model =
    if model.AdminViewUser.IsNone
    then VerifyChangeUserParamsEle (Email, "", InputEmail)
    else VerifyAdminChangeUserParamsEle (model.AdminViewUser.Value, Email, "", InputEmailAdmin)
let extraElementPassword model =
    if model.AdminViewUser.IsNone
    then VerifyChangeUserParamsEle (Password, "", InputPw)
    else VerifyAdminChangeUserParamsEle (model.AdminViewUser.Value, Password, "", InputPwAdmin)
let extraElementRole model =
    VerifyAdminChangeUserParamsEle (model.AdminViewUser.Value, Role, "", InputRoleAdmin)

let dangerZone model dispatch user=
    let deleteMsg =
        if model.AdminViewUser.IsNone
        then (fun _ ->
            dispatch (UpdateMainExtraElement
                (DeleteAccountRequestEle (DeleteAccountText))
            )
        )
        else ( fun _ ->
            dispatch (UpdateMainExtraElement
                (AdminDeleteAccountRequestEle (model.AdminViewUser.Value, AdminDeleteAccountText))
            )
        )
    Columns.columns [
        Columns.Props [
            Style [
                Border "1.5px solid #ff4d4d"
                BorderRadius "3px"
                MarginTop "10px"
    ]]] [
        Column.column [][
            Heading.h6 [][str "Delete this user account"]
            Heading.h6 [Heading.IsSubtitle] [ (if user = model.User.Value then str "Once you delete your account, there is no going back. Please be certain" else str "Once you delete this account, there is no going back. Please be certain")]
        ]
        Column.column [Column.Width (Screen.All,Column.IsOneFifth);Column.Modifiers [Modifier.TextAlignment (Screen.All,TextAlignment.Right)]][
            Button.button [Button.Color IsDanger;Button.OnClick deleteMsg][
                str "Delete"
            ]
        ]
    ]

let userElement model dispatch (user:User) =
    let userNameColumn =
        userAccountinformationColumn "Name" user.Username (fun _ -> dispatch (UpdateMainExtraElement (extraElementUserName model)))
    let emailColumn =
        userAccountinformationColumn "E-Mail" user.Email (fun _ -> dispatch (UpdateMainExtraElement (extraElementEmail model)))
            
    [
        userNameColumn
        emailColumn
        Columns.columns [][
            Column.column [][
                Heading.h5 [][str "Password"]
                Heading.h6 [Heading.IsSubtitle] [str "******"]
            ]
            Column.column [ Column.Modifiers [Modifier.TextAlignment (Screen.All,TextAlignment.Right)] ][
                Button.a [ Button.OnClick (fun _ -> dispatch (UpdateMainExtraElement (extraElementPassword model))) ][str "Change"]
            ]
        ]
        Columns.columns [][
            Column.column [][
                Heading.h5 [][str "Role"]
                Heading.h6 [Heading.IsSubtitle][str (string user.Role)]
            ]
            Column.column [ Column.Modifiers [Modifier.TextAlignment (Screen.All,TextAlignment.Right)] ][
                Button.a [Button.OnClick (fun _ -> dispatch (UpdateMainExtraElement (extraElementRole model)))][str "Change"]
            ]
        ]
        Columns.columns [][
            Column.column [][
                Heading.h5 [][str "Account Origin"]
                Heading.h6 [Heading.IsSubtitle][str user.AccountOrigin]
            ]
        ]
        Columns.columns [][
            Column.column [][
                Heading.h5 [][str "Unique Identifier"]
                Heading.h6 [Heading.IsSubtitle][str user.UniqueId]
            ]
        ]
        dangerZone model dispatch user
    ]

let extUserElement model dispatch (user:User) =
    let userNameColumn =
        userAccountinformationColumn "Name" user.Username (fun _ -> dispatch (UpdateMainExtraElement (extraElementUserName model)))
    let emailColumn =
        userAccountinformationColumn "E-Mail" user.Email (fun _ -> dispatch (UpdateMainExtraElement (extraElementEmail model)))
    [
        userNameColumn
        emailColumn
        Columns.columns [ ] [
            Column.column [][
                Heading.h5 [][str "Role"]
                Heading.h6 [Heading.IsSubtitle][str (string user.Role)]
            ]
            Column.column [
                Column.Modifiers [
                    Modifier.TextAlignment (Screen.All,TextAlignment.Right);
                    Modifier.IsHidden (Screen.All , (user = model.AdminViewUser.Value |> not))
                ]
            ][
                Button.a [Button.OnClick (fun _ -> dispatch (UpdateMainExtraElement (extraElementRole model)))][str "Change"]
            ]
        ]
        Columns.columns [][
            Column.column [][
                Heading.h5 [][str "Account Origin"]
                Heading.h6 [Heading.IsSubtitle][str user.AccountOrigin]
            ]
        ]
        Columns.columns [][
            Column.column [][
                Heading.h5 [][str "Unique Identifier"]
                Heading.h6 [Heading.IsSubtitle][str user.UniqueId]
            ]
        ]
        dangerZone model dispatch user 
    ]

let userAccountElement model (dispatch : Msg -> unit) =
    let user =
        if model.AdminViewUser.IsNone then model.User.Value else model.AdminViewUser.Value

    let userAccountEle =
        if model.User.IsNone || model.Authenticated = false
        then
            [
                Columns.columns [][
                    Column.column [][
                        Heading.h5 [][str "Access Denied" ]
                    ]
                ]
            ]
        else
            if user.ExtLogin.IsTrue then extUserElement model dispatch user else userElement model dispatch user

    let backButtonRequest =
        if model.AdminViewUser.IsNone then
            str ""
        else
            Button.button [
                Button.Props [
                    Style [
                        Position PositionOptions.Fixed
                        Bottom "50%"
                        Left "5%"] ]
                Button.OnClick (fun _ -> dispatch NavigateUserList)
            ][
                Icon.icon [ ][
                    span [ClassName "fas fa-arrow-left"] []
                ]
                span [][str " Back"]
            ]

    div [Style [MarginTop "5%";MarginBottom "5%"]][
        //(extraEle model dispatch)
        Column.column [ Column.Width (Screen.All,Column.IsHalf);Column.Offset (Screen.All,Column.IsOneQuarter) ][
            Box.box' [Props[Style[Padding @"5% 5% 5% 5%"]]]
                userAccountEle
            backButtonRequest
        ]
    ]

type Props = {
    Model: Model
    Dispatch: Msg -> unit
}

let view = elmishView "UserAccount" <| fun { Model = model; Dispatch = dispatch } ->
    userAccountElement model dispatch
    //str "Welcome to user account"