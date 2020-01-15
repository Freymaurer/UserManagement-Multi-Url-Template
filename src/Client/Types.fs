module Client.Types

open System

open Elmish
open Elmish.UrlParser
open Fable.Core
open Fable.React

open Shared
open Client.AuxFunctions

//// multi url part with routing ////

type Route =
    //| Root
    | Home
    | Counter
    /// Route for userAccount viewed either by user or by admin ("user"/"admin",userId).
    | UserAccount of string * string
    | UserList
    | Detail of int

let toRouteUrl route =
    match route with
    //| Route.Root -> "/"
    | Route.Home-> "/home"
    | Route.Counter -> "/counter"
    | Route.UserAccount (role,userId) -> "/" + role +  "/account/" + userId
    | Route.UserList -> "/admin/userlist"
    | Route.Detail id -> "/detail/" + string id

    ///explained here: https://elmish.github.io/browser/routing.html
let curry f x y = f (x,y)

type PageModel =
| RootModel
| HomeModel of Home.Model
| CounterModel of Counter.Model
| UserAccountModel of UserAccount.Model
| UserListModel of UserList.Model

module Routing =

    open Elmish.UrlParser
    open Elmish.Navigation

    let route =
        oneOf [
            map Route.Home (s "home")
            map Route.Counter (s "counter")
            map (curry Route.UserAccount) (str </> s "account" </> str)
            map Route.UserList (s "admin" </> s "userlist")
            map (fun detailID -> Route.Detail detailID) (s "detail" </> i32)
        ]

    // Take a window.Location object and return an option of Route

    let parsePath (location:Browser.Types.Location) : Route option = UrlParser.parsePath route location

//// end of multi url part ////

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
    CurrentRoute : Route option
    CurrentPageModel : PageModel
    Debug : string option
    NumberForDetail : int
    Counter: Counter option
    ErrorMsg : string option
    InputString : string
    LoginModel : LoginModel
    RegisterModel : RegisterModel
    User : User option
    Loading : bool
    Authenticated : bool
    ExtraReactElement : ExtraReactElement<Msg,Model>
    //MainReactElement : MainReactElement
    ShowMenuBool : bool
    AdminUserList : User []
    AdminUserListRoleFilter : ActiveUserRoles
    AdminViewUser : User option
    AdminAssignRole : ActiveUserRoles
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
and Msg =
    // Client Msg
    | Navigate of Route
    | HomeMsg of Home.Msg
    | CounterMsg of Counter.Msg
    | UserAccountMsg of UserAccount.Msg
    | UserListMsg of UserList.Msg
    | UpdateNumberForDetail of int
    | ClearRegisterLogin
    | ToggleMenu
    //| ChangeMainReactElement of MainReactElement
    | SortAllUserList of string
    | FilterAllUserList of ActiveUserRoles
    | AdminSelectUser of User
    | AdminSelectAssignRole of ActiveUserRoles
    | Increment
    | Decrement
    | UpdateInputString of string
    | UpdateExtraElement of ExtraReactElement<Msg,Model>
    | UpdateLoginUsername of string
    | UpdateLoginUserPw of string
    // Server Msg
    | InitialCountLoaded of Counter
    | InitialUserLoaded of User
    | UpdateRegisterModel of RegisterModel
    | DotnetRegisterRequest of RegisterModel
    | DotnetRegisterResponse of Result<DotnetRegisterResults,exn>
    | DotnetLoginRequest of LoginModel
    | DotnetLoginResponse of Result<DotnetLoginResults,exn>
    | DotnetGetUserRequest
    | DotnetGetUserResponse of Result<User,exn>
    | DotnetLogOutRequest
    | DotnetLogOutResponse of Result<DotnetLogOutResults,exn>
    | DotnetChangeUserParamRequest of LoginModel * UserParameters * string
    | DotnetChangeUserParamResponse of Result<DotnetChangeParameterResults,exn>
    | GetUserCounterRequest
    | GetUserCounterResponse of Result<Counter,exn>
    | DeleteAccountRequest of LoginModel
    | DeleteAccountResponse of Result<DotnetDeleteAccountResults,exn>
    | AddUsernameToExtLogin of string
    | AddUsernameToExtLoginResponse of Result<DotnetChangeParameterResults,exn>
    | AdminGetAllUsersRequest
    | AdminGetAllUsersResponse of Result<User [],exn>
    | AdminRegisterUserRequest of RegisterModel * ActiveUserRoles
    | AdminRegisterUserResponse of Result<DotnetRegisterResults,exn>
    | AdminChangeUserParamsRequest of LoginModel * User * UserParameters * string
    | AdminChangeUserParamsResponse of Result<DotnetChangeParameterResults,exn>
    | AdminDeleteAccountRequest of LoginModel * User
    | AdminDeleteAccountResponse of Result<DotnetDeleteAccountResults,exn>
    /// Msgs used for development
    | Debug of string
    | GetContextClaimsRequest
    | GetContextClaimsResponse of Result<string,exn>

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

