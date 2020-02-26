
module Client.State
open System

open Elmish
open Elmish.Navigation
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fulma
open Shared

open Client.AuxFunctions
open Client.Types

let urlUpdate (route: Route option) (model:Model) =
    let logInCmd =
        Cmd.OfAsync.perform
            Server.dotnetSecureApi.dotnetGetUser
            ()
            InitialUserLoaded
    match route with
    | Some Route.Home ->
        let m, cmd = Home.init ()
        { model with
            CurrentPageModel = PageModel.HomeModel m
            CurrentRoute = route }, Cmd.batch [logInCmd; Cmd.map HomeMsg cmd]
    | Some Route.Counter ->
        let m, cmd = Counter.init ()
        { model with
            CurrentPageModel = PageModel.CounterModel m
            CurrentRoute = route }, Cmd.batch [logInCmd; Cmd.map CounterMsg cmd]
    | Some (Route.UserAccount (role, userId)) ->
        let m, cmd = UserAccount.init (role, userId)
        { model with
            CurrentPageModel = PageModel.UserAccountModel m
            CurrentRoute = route }, Cmd.batch [Cmd.map UserAccountMsg cmd]
    | Some Route.UserList ->
        let m, cmd = UserList.init ()
        { model with
            CurrentPageModel = PageModel.UserListModel m
            CurrentRoute = route }, Cmd.map UserListMsg cmd
    | Some (Route.Detail id) ->
            { model with
                CurrentRoute = route
                NumberForDetail = id
                Debug = Some ( sprintf "you just connected to detail number %i" id )
            }, logInCmd
    | None ->
        printfn "No Route found! Route: %A" route
        model, Cmd.ofMsg (Navigate Route.Home)
    | _ ->
        { model with CurrentRoute = route }, Cmd.none

// defines the initial state and initial command (= side-effect) of the application
let init _ =
    let initialModel = {
        CurrentRoute = None
        CurrentPageModel = RootModel
        Debug = None
        NumberForDetail = 0
        Counter = None
        ErrorMsg = None
        InputString = ""
        User = None
        Loading = true
        Authenticated = false
        LoginModel = {Username = ""; Password = ""}
        RegisterModel = {Username = "";Password = "";Email = ""}
        ExtraReactElement = EmptyElement
        //MainReactElement = Counter
        ShowMenuBool = false
        AdminUserList = [||]
        AdminUserListRoleFilter = All
        AdminViewUser = None
        AdminAssignRole = NoRole
    }
    let route = Routing.parsePath Browser.Dom.document.location
    urlUpdate route initialModel

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg, currentModel.CurrentPageModel with
    | Navigate route , _  ->
        currentModel, Navigation.newUrl (toRouteUrl route)
    | HomeMsg msg, HomeModel m ->
        let m, cmd =
            Home.update msg m
        let nextModel = {
            currentModel with
                CurrentPageModel = HomeModel m
                CurrentRoute = Some Home
            }
        nextModel, Cmd.map HomeMsg cmd
    | CounterMsg msg, CounterModel m ->
        let m, cmd =
            Counter.update msg m
        let nextModel = {
            currentModel with
                CurrentPageModel = CounterModel m
                CurrentRoute = Some Route.Counter
            }
        nextModel, Cmd.map CounterMsg cmd
    | UserAccountMsg (UserAccount.UpdateMainExtraElement ele), UserAccountModel m ->
        let nextModel = {
            currentModel with
                ExtraReactElement = UserAccountExtraElements ele
                InputString = ""
                RegisterModel = {Username = "";Password = "";Email = ""}
                AdminAssignRole = NoRole
        }
        nextModel, Cmd.none
    | UserAccountMsg UserAccount.Msg.NavigateUserList, UserAccountModel m ->
        let navigate = Cmd.ofMsg (Navigate Route.UserList)
        currentModel, navigate
    | UserAccountMsg msg, UserAccountModel m ->
        let mNew, cmd =
            UserAccount.update msg m
        let nextModel = {
            currentModel with
                CurrentPageModel = UserAccountModel mNew
            }
        nextModel, Cmd.map UserAccountMsg cmd
    | UserListMsg (UserList.UpdateMainExtraElement ele), UserListModel m ->
        let nextModel = {
            currentModel with
                ExtraReactElement = UserListExtraElements ele
                InputString = ""
                RegisterModel = {Username = "";Password = "";Email = ""}
                AdminAssignRole = NoRole
        }
        nextModel, Cmd.none
    | UserListMsg (UserList.AdminNavigateToUserPage user), UserListModel m ->
        let m, cmd =
            UserAccount.init ("admin",user.UniqueId)
        let changeUrl =
            Cmd.ofMsg (Navigate (Route.UserAccount ("admin", user.UniqueId)))
        let aggregateMsgs =
            Cmd.batch [changeUrl; Cmd.map UserAccountMsg cmd]
        let nextModel = {
            currentModel with
                CurrentPageModel = UserAccountModel m
                AdminViewUser = Some user
        }
        nextModel, aggregateMsgs

    | UserListMsg msg, UserListModel m ->
        let mNew, cmd =
            UserList.update msg m
        let nextModel = {
            currentModel with
                CurrentPageModel = UserListModel mNew
                CurrentRoute = Some (Route.UserList)
        }
        nextModel, Cmd.map UserListMsg cmd
    | UpdateNumberForDetail (inputVal), _ ->
        let nextModel = {
            currentModel with
                NumberForDetail = inputVal
        }
        nextModel,Cmd.none
        ///
    | UpdateInputString (str), _ ->
        let nextModel = { currentModel with InputString = str }
        nextModel,Cmd.none
        ///
    | InitialCountLoaded initialCount, _ ->
        let nextModel = { currentModel with Counter = Some initialCount; Loading = false }
        nextModel, Cmd.none
        ///
    | InitialUserLoaded initialUser, _ ->
        let nextModel =
            if initialUser.ExtLogin.IsTrue && initialUser.ExtLogin.IsUsernameSet = false
            then
                { currentModel with User = Some initialUser; Loading = false;Authenticated = false; ExtraReactElement = AddUsernameToExternLoginModal }
            else
                { currentModel with User = Some initialUser; Loading = false;Authenticated = true }
        nextModel, Cmd.none
        // Menu Management Functions
        ///
    | ClearRegisterLogin , _->
        let nextModel = {
            currentModel with
                LoginModel = {Username = ""; Password = ""}
                RegisterModel = {Username = "";Password = "";Email = ""}
                AdminAssignRole = NoRole
        }
        nextModel, Cmd.none
        ///
    | ToggleMenu, _ ->
        let nextModel = {
            currentModel with ShowMenuBool = if currentModel.ShowMenuBool = true then false else true
        }
        nextModel,Cmd.none
        ///
    //| ChangeMainReactElement (newElement), _ ->
    //    let nextModel = {
    //        currentModel with
    //            MainReactElement = newElement
    //            ShowMenuBool = false
    //    }
    //    nextModel, Cmd.none
        ///
    | SortAllUserList (searchString), _ ->
        let sortedArr =
            currentModel.AdminUserList
            |> Array.sortByDescending (
                fun x ->
                    let completeInfo = x.Username + " " + x.Email
                    Client.AuxFunctions.rankCompareStringsBySearchString searchString completeInfo
            )
        let nextModel = { currentModel with AdminUserList = sortedArr }
        nextModel,Cmd.none
        ///
    | FilterAllUserList (userRole), _ ->
        let nextModel = { currentModel with AdminUserListRoleFilter = userRole }
        nextModel,Cmd.none
        ///
    | AdminSelectUser (user), _ ->
        let nextModel = {
            currentModel with
                AdminViewUser = Some user
                //MainReactElement = UserAccount user
        }
        nextModel,Cmd.none
        ///
    | AdminSelectAssignRole (role), _ ->
        let nextModel = { currentModel with AdminAssignRole = role }
        nextModel,Cmd.none
        // functions to manage input fields for user log in
        ///
    | UpdateLoginUsername (name:string), _ ->
        let nextModel = {
            currentModel with LoginModel = {currentModel.LoginModel with Username = name}
        }
        nextModel, Cmd.none
        ///
    | UpdateLoginUserPw (pw:string), _ ->
        let nextModel = {
            currentModel with LoginModel = {currentModel.LoginModel with Password = pw}
        }
        nextModel, Cmd.none
        ///
    | UpdateRegisterModel (registerModel), _ ->
        let nextModel = { currentModel with RegisterModel = registerModel }
        nextModel, Cmd.none
        ///
    | UpdateExtraElement (element), _ ->
        let nextModel = { currentModel with ExtraReactElement = element }
        nextModel,Cmd.none
        // functions to handle user registration
        ///
    | DotnetRegisterRequest (registermodel), _ ->
        let nextModel = { currentModel with ExtraReactElement = EmptyElement }
        let cmd =
            Cmd.OfAsync.either
                Server.userApi.dotnetRegister
                (registermodel)
                (Ok >> DotnetRegisterResponse)
                (Error >> DotnetRegisterResponse)
        nextModel,cmd
        ///
    | DotnetRegisterResponse (Ok value), _ ->
        let nextModel,cmd =
            match value with
            | RegisterFail x ->
                {
                    currentModel with
                        Loading = false
                        ExtraReactElement = Message x
                        RegisterModel = {currentModel.RegisterModel with Password = ""}
                }
                , Cmd.none
            | RegisterSuccess ->
                { currentModel with Loading = false } , Cmd.ofMsg DotnetGetUserRequest
        nextModel, cmd
        ///
    | DotnetRegisterResponse (Error e), _ ->
        let nextModel = {
            currentModel with
                Loading = false
                ErrorMsg = Some e.Message
                ExtraReactElement = Message e.Message
                RegisterModel = {currentModel.RegisterModel with Password = ""}
        }
        nextModel, Cmd.none
        // functions to log in user via asp.net
        ///
    | DotnetLoginRequest (user), _ ->
        let nextModel = { currentModel with Loading = true }
        let cmdLogin =
            Cmd.OfAsync.either
                Server.userApi.dotnetLogin
                user
                (Result.Ok >> DotnetLoginResponse)
                (Result.Error >> DotnetLoginResponse)
        nextModel,cmdLogin
        ///
    | DotnetLoginResponse (Result.Error e), _ ->
        let nextModel = {
            currentModel with
                ErrorMsg = Some e.Message
                Loading = false
                LoginModel = {currentModel.LoginModel with Password = ""}
                //MainReactElement = Counter
        }
        nextModel,Cmd.ofMsg (UpdateExtraElement (Message e.Message))
        ///
    | DotnetLoginResponse (Result.Ok value), _ ->
        let (nextModel,cmd) =
            match value with
            | LoginSuccess ->
                {
                    currentModel with
                        Loading = false
                        //MainReactElement = Counter
                        LoginModel = {Username = ""; Password = ""}
                },
                Cmd.ofMsg DotnetGetUserRequest
            | LoginFail msg ->
                {
                    currentModel with
                        ErrorMsg = Some msg
                        Loading = false
                        //MainReactElement = Counter
                        LoginModel = {Username = ""; Password = ""}
                },
                Cmd.ofMsg (UpdateExtraElement (Message msg))
        nextModel, cmd
        // functions to access already logged in user information
        ///
    | DotnetGetUserRequest, _ ->
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.dotnetGetUser
                ()
                (Result.Ok >> DotnetGetUserResponse)
                (Error >> DotnetGetUserResponse)
        currentModel, cmd
        ///
    | DotnetGetUserResponse (Ok value), _ ->
        let nextModel =
            if value.ExtLogin.IsTrue && value.ExtLogin.IsUsernameSet = false
            then { currentModel with
                    Authenticated = true; User = Some value; Loading = false; ExtraReactElement = AddUsernameToExternLoginModal }
            else { currentModel with
                    Authenticated = true; User = Some value; Loading = false }
        nextModel,Cmd.none
        ///
    | DotnetGetUserResponse (Error e), _ ->
        let nextModel = {
            currentModel with
                ExtraReactElement = Message "Getting User Information failed"
                Loading = false
                ErrorMsg = Some e.Message
        }
        nextModel,Cmd.none
        // functions to access user-only counter
        ///
    //| GetUserCounterRequest, _ ->
    //    let nextModel = { currentModel with Loading = true }
    //    let cmd =
    //        Cmd.OfAsync.either
    //            Server.dotnetSecureApi.getUserCounter
    //            ()
    //            (Ok >> GetUserCounterResponse)
    //            (Error >> GetUserCounterResponse)
    //    nextModel, cmd
    //    ///
    //| GetUserCounterResponse (Ok value), _ ->
    //    let nextModel = { currentModel with Loading = false; Counter = Some value }
    //    nextModel, Cmd.none
    //    ///
    //| GetUserCounterResponse (Error e), _ ->
    //    let nextModel = {
    //        currentModel with
    //            Loading = false
    //            ErrorMsg = Some e.Message
    //            ExtraReactElement = Message "This function is for User only"
    //    }
    //    nextModel, Cmd.none
    //    // functions to handle user log out
    //    ///
    | DotnetLogOutRequest, _ ->
        let nextModel = { currentModel with Loading = true }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.dotnetUserLogOut
                ()
                (Ok >> DotnetLogOutResponse)
                (Error >> DotnetLogOutResponse)
        nextModel, cmd
        ///
    | DotnetLogOutResponse (Ok value), _ ->
        let cmd =
            Cmd.ofMsg (Navigate Route.Home)

        let initModel,cmd' = init()
        let nextModel =
            {initModel with ErrorMsg = Some (string value)}
        nextModel, Cmd.batch  [cmd;cmd']
        ///
    | DotnetLogOutResponse (Error e), _ ->
        let nextModel = { currentModel with ErrorMsg = Some e.Message }
        nextModel, Cmd.none
        ///
    | DotnetChangeUserParamRequest (loginModel, userParam, input), _ ->
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.dotnetChangeUserParameters
                (loginModel, userParam, input)
                (Ok >> DotnetChangeUserParamResponse)
                (Error >> DotnetChangeUserParamResponse)
        let nextModel = {
            currentModel with
                Loading = true
        }
        nextModel, cmd
        ///
    | DotnetChangeUserParamResponse (Ok value), _ ->
        match value with
        | ChangeParamSuccess ->
            let init,cmd = UserAccount.init("user",currentModel.User.Value.UniqueId)
            let nextModel = {
                currentModel with
                    Loading = false
                    LoginModel = {Username = ""; Password = ""}
                    ExtraReactElement = EmptyElement
                    CurrentPageModel = UserAccountModel init
                }
            let nav = Cmd.map UserAccountMsg cmd
            let getUser = Cmd.ofMsg DotnetGetUserRequest
            nextModel, Cmd.batch [getUser;nav]
        | ChangeParamFail str ->
            let nextModel = {
                currentModel with
                    Loading = false
                    ExtraReactElement = Message str
                    LoginModel = {currentModel.LoginModel with Password = ""}
                }
            nextModel, Cmd.none
        ///
    | DotnetChangeUserParamResponse (Error e), _ ->
        let nextModel = { currentModel with ExtraReactElement = Message e.Message }
        nextModel, Cmd.none
        ///
    | DeleteAccountRequest (loginModel), _ ->
        let nextModel = { currentModel with Loading = true }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.dotnetDeleteUserAccount
                loginModel
                (Ok >> DeleteAccountResponse)
                (Error >> DeleteAccountResponse)
        nextModel, cmd
        ///
    | DeleteAccountResponse (Ok value), _ ->
        let initModel,cmd = init()
        let nav = Cmd.ofMsg (Navigate Route.Home)
        let accCmd = Cmd.batch [cmd; nav]
        let nextModel,cmd' =
            match value with
            | DeleteSuccess ->
                {initModel with ExtraReactElement = Message (sprintf "Account and all related information was deleted! If you were unhappy with our service please tell us about it at %s" ServiceHelpers.ServiceMail)}, accCmd
            | DeleteFail str ->
                Browser.Dom.window.alert ("You tried deleting the account and it failed. For security reasons you were logged out. " + str)
                currentModel, Cmd.ofMsg DotnetLogOutRequest
        nextModel, cmd'
        ///
    | DeleteAccountResponse (Error e), _ ->
        let nextModel = {
            currentModel with
                ExtraReactElement = Message e.Message
                LoginModel = {Username = ""; Password = ""}
        }
        nextModel,Cmd.none
        ///
    | AddUsernameToExtLogin (username), _ ->
        let nextModel = { currentModel with Loading = true }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.addUsernameToExtLogin
                (username)
                (Ok >> AddUsernameToExtLoginResponse)
                (Error >> AddUsernameToExtLoginResponse)
        nextModel,cmd
        ///
    | AddUsernameToExtLoginResponse (Ok value), _ ->
        match value with
        | ChangeParamSuccess ->
            init()
        | ChangeParamFail e ->
            Browser.Dom.window.alert (sprintf "Registering your new username failed! %s" e)
            init()
        ///
    | AddUsernameToExtLoginResponse (Error e), _ ->
        let nextModel = { currentModel with Loading = false; ExtraReactElement = Message e.Message}
        nextModel, Cmd.none
        ///
    | AdminGetAllUsersRequest, _ ->
        let nextModel = { currentModel with Loading = true }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetAdminSecureApi.dotnetGetAllUsers
                    ()
                    (Ok >> AdminGetAllUsersResponse)
                    (Error >> AdminGetAllUsersResponse)
        nextModel,cmd
        ///
    | AdminGetAllUsersResponse (Ok value), _ ->
        let nextModel = {
            currentModel with
                Loading = false
                AdminUserList = value
        }
        nextModel, Cmd.none
        ///
    | AdminGetAllUsersResponse (Error e), _ ->
        let nextModel = {
            currentModel with
                Loading = false
                ErrorMsg = Some e.Message
                ExtraReactElement = Message e.Message
        }
        nextModel, Cmd.none
        ///
    | AdminRegisterUserRequest (registermodel,role), _ ->
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetAdminSecureApi.adminRegisterUser
                    (registermodel,role)
                    (Ok >> AdminRegisterUserResponse)
                    (Error >> AdminRegisterUserResponse)
        let nextModel = {
            currentModel with
                Loading = true
                ExtraReactElement = EmptyElement
        }
        nextModel, cmd
        ///
    | AdminRegisterUserResponse (Ok value), _ ->
        let nextModel,cmd =
            match value with
            | RegisterSuccess ->
                let m, cmd = UserList.init ()
                let nextModel = {
                    currentModel with
                        Loading = false
                        CurrentPageModel = UserListModel m
                        CurrentRoute = Some Route.UserList
                        RegisterModel = {Username = "";Password = "";Email = ""}
                }
                nextModel , Cmd.map UserListMsg cmd
            | RegisterFail x ->
                {
                    currentModel with
                        Loading = false
                        ExtraReactElement = Message x
                        RegisterModel = {currentModel.RegisterModel with Password = ""}
                }
                , Cmd.none
        nextModel, cmd
        ///
    | AdminRegisterUserResponse (Error e), _ ->
        let nextModel = {
            currentModel with
                Loading = false
                ErrorMsg = Some e.Message
                ExtraReactElement = Message e.Message
                RegisterModel = {currentModel.RegisterModel with Password = ""}
        }
        nextModel, Cmd.none
        ///
    | AdminChangeUserParamsRequest (loginModel,user,userParam,input), _ ->
        let nextModel = {
            currentModel with
                Loading = true;
                ExtraReactElement = EmptyElement;
                LoginModel = {Username = "";Password = ""}
        }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetAdminSecureApi.adminChangeUserParameters
                (loginModel,user,userParam,input)
                (Ok >> AdminChangeUserParamsResponse)
                (Error >> AdminChangeUserParamsResponse)
        nextModel, cmd
        ///
    | AdminChangeUserParamsResponse (Ok value), _ ->
        match value with
        | ChangeParamSuccess ->
            let nextModel = {
                currentModel with
                    Loading = false
                    AdminViewUser = None
                    InputString = ""
            }
            let aggregateMsgs =
                Cmd.batch [Cmd.ofMsg (Navigate Route.UserList); Cmd.ofMsg AdminGetAllUsersRequest]
            nextModel, aggregateMsgs
        | ChangeParamFail str ->
            let nextModel = {
                currentModel with
                    Loading = false
                    ExtraReactElement = Message str
                    InputString = ""
            }
            nextModel,Cmd.none
        ///
    | AdminChangeUserParamsResponse (Error e), _ ->
        let nextModel = {
            currentModel with
                Loading = false
                ExtraReactElement = Message e.Message
        }
        nextModel,Cmd.none
        ///
    | AdminDeleteAccountRequest (loginModel,user), _ ->
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetAdminSecureApi.adminDeleteAccount
                (loginModel,user)
                (Ok >> AdminDeleteAccountResponse)
                (Error >> AdminDeleteAccountResponse)
        let nextModel = {
            currentModel with
                Loading = true
                LoginModel = {Username = "";Password = ""}
                ExtraReactElement = EmptyElement
        }
        nextModel,cmd
        ///
    | AdminDeleteAccountResponse (Ok value), _ ->
        let nextModel,cmd =
            match value with
            | DeleteSuccess ->
                let nextModel = {
                    currentModel with
                        Loading = false
                        AdminViewUser = None
                }
                let aggregateMsgs =
                    Cmd.batch [Cmd.ofMsg (Navigate Route.UserList); Cmd.ofMsg AdminGetAllUsersRequest]
                nextModel, aggregateMsgs
            | DeleteFail str ->
                Browser.Dom.window.alert ("You tried deleting the account and it failed. For security reasons you were logged out. " + str)
                currentModel, Cmd.ofMsg DotnetLogOutRequest
        nextModel, cmd
        ///
    | AdminDeleteAccountResponse (Error e), _ ->
        let nextModel = {
            currentModel with
                ExtraReactElement = Message e.Message
                LoginModel = {Username = ""; Password = ""}
        }
        nextModel,Cmd.none
        // the following was used during development
        ///
    | GetContextClaimsRequest, _ ->
        let cmd =
            Cmd.OfAsync.either
                Server.userApi.getContextClaims
                ()
                (Ok >> GetContextClaimsResponse)
                (Error >> GetContextClaimsResponse)
        currentModel, cmd
        ///
    | GetContextClaimsResponse (Ok value), _ ->
        let nextModel = {
            currentModel with ExtraReactElement = Message ("Connection was stable and succeded >=> " + value)
        }
        nextModel,Cmd.none
        ///
    | GetContextClaimsResponse (Error e), _ ->
        let nextModel = { currentModel with ExtraReactElement = Message e.Message }
        nextModel, Cmd.none
        ///
    | Debug (message), _ ->
        { currentModel with ErrorMsg = Some message}, Cmd.none
    | _ -> currentModel, Cmd.none