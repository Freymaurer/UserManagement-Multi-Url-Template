module Client.UserList

open Fable.React
open Fable.React.Props
open Fulma
open Shared
open Fable.FontAwesome

open Shared
open Client.AuxFunctions
open Client.View.Helper
open Elmish

type Model = {
    ErrorMsg : string option
    InputString : string
    LoginModel : LoginModel
    RegisterModel : RegisterModel
    User : User option
    Loading : bool
    Authenticated : bool
    ExtraReactElement : ExtraReactElement<Msg,Model>
    AdminUserList : User []
    AdminUserListRoleFilter : ActiveUserRoles
    AdminViewUser : User option
    }


// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
and Msg =
    | SortAllUserList of string
    | FilterAllUserList of ActiveUserRoles
    | AdminSelectAssignRole of ActiveUserRoles
    | UpdateExtraElement of ExtraReactElement<Msg,Model>
    // Server Msg
    | InitialUserLoaded of User
    | AdminGetAllUsersRequest
    | AdminGetAllUsersResponse of Result<User [],exn>
    | UpdateMainExtraElement of UserListAux.ExtraReactElements
    | AdminNavigateToUserPage of User

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
let init _ =
    let initialModel = {
        ErrorMsg = None
        InputString = ""
        User = None
        Loading = true
        Authenticated = false
        LoginModel = {Username = ""; Password = ""}
        RegisterModel = {Username = "";Password = "";Email = ""}
        ExtraReactElement = EmptyElement
        AdminUserList = [||]
        AdminUserListRoleFilter = All
        AdminViewUser = None
    }
    //let logInCmd =
    //    Cmd.OfAsync.perform
    //        Server.dotnetSecureApi.dotnetGetUser
    //        ()
    //        InitialUserLoaded
    let getUserListReq =
        Cmd.ofMsg Msg.AdminGetAllUsersRequest
    initialModel, Cmd.batch [getUserListReq]


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
                { currentModel with User = Some initialUser; Loading = false;Authenticated = false; ExtraReactElement = AddUsernameToExternLoginModal }
            else
                { currentModel with User = Some initialUser; Loading = false;Authenticated = true }
        nextModel, Cmd.none
        ///
    | SortAllUserList (searchString) ->
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
    | FilterAllUserList (userRole) ->
        let nextModel = { currentModel with AdminUserListRoleFilter = userRole }
        nextModel,Cmd.none
        ///
    | UpdateExtraElement (element) ->
        let nextModel = { currentModel with ExtraReactElement = element }
        nextModel,Cmd.none
        ///
    | AdminGetAllUsersRequest ->
        let nextModel = { currentModel with Loading = true }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetAdminSecureApi.dotnetGetAllUsers
                    ()
                    (Ok >> AdminGetAllUsersResponse)
                    (Error >> AdminGetAllUsersResponse)
        nextModel,cmd
        ///
    | AdminGetAllUsersResponse (Ok value) ->
        let nextModel = {
            currentModel with
                Loading = false
                AdminUserList = value
        }
        nextModel, Cmd.none
        ///
    | AdminGetAllUsersResponse (Error e) ->
        let nextModel = {
            currentModel with
                Loading = false
                ErrorMsg = Some e.Message
                ExtraReactElement = Message e.Message
        }
        nextModel, Cmd.none
        ///
    | _ -> currentModel, Cmd.none

////////////////////////////////////////////////////////////////
///         View for User List Information      ////////////////
////////////////////////////////////////////////////////////////

let displayUser (user:User) dispatch =
    [|
        tr [][
            td [][str user.Username]
            td [][str user.Email]
            td [][str (string user.Role)]
            span [Style [Padding "auto";MarginLeft "1.5rem"]][
                Button.span [
                    Button.Size IsSmall
                    Button.OnClick (fun _ ->
                        dispatch (
                            /// this function is only mentionen in UserList.Msg, but the complete function can be found
                            /// in State.update, where it is intercepted.
                            AdminNavigateToUserPage user
                        )
                    )
                ] [
                    str "Edit"
                ]
            ]
        ]
    |]

let dropdownNavbarButtonSize (nameStr:string) dispatchElement =
    Navbar.Item.a
        [ Navbar.Item.Props [ Props.OnClick dispatchElement ];Navbar.Item.CustomClass "dropdownFilter" ]
        [ str nameStr]

let displayAllUsersNavbar model dispatch =
    Navbar.navbar [ Navbar.Props [ Style [
        PaddingTop "0.5%";BorderBottom "1px solid lightgrey"; MarginBottom "0.5%";
        JustifyContent "center"; ZIndex "5"
    ]]] [
        Navbar.Item.a [ Navbar.Item.Props [Style [Width "25%"]]][
            Input.search [
                Input.Size Size.IsSmall
                Input.Placeholder "...search"
                Input.Props [Style [Height "100%"]]
                Input.OnChange (fun e -> dispatch (SortAllUserList e.Value))
            ]
        ]
        Navbar.navbar [Navbar.Props [Style [Width "25%";]]][
            Navbar.Item.a [
                Navbar.Item.Props [Style [MarginLeft "auto";Padding "3px"]]
                Navbar.Item.HasDropdown; Navbar.Item.IsHoverable;
            ] [
                Navbar.Link.a [] [ str (if model.AdminUserListRoleFilter = All then "Role-Filter" else string model.AdminUserListRoleFilter) ]
                Navbar.Dropdown.div [ ] [
                    dropdownNavbarButtonSize "All" (fun _ -> dispatch (FilterAllUserList All))
                    Dropdown.divider []
                    dropdownNavbarButtonSize "User" (fun _ -> dispatch (FilterAllUserList User))
                    dropdownNavbarButtonSize "UserManager" (fun _ -> dispatch (FilterAllUserList UserManager))
                    dropdownNavbarButtonSize "Admin" (fun _ -> dispatch (FilterAllUserList Admin))
                    dropdownNavbarButtonSize "Developer" (fun _ -> dispatch (FilterAllUserList Developer))
                ]
            ]
        ]
        Navbar.Item.a [][
            div [
                OnClick (
                    fun _ ->
                        //dispatch (UpdateExtraElement AdminRegisterModal)
                        dispatch (UpdateMainExtraElement UserListAux.ExtraReactElements.AdminRegisterModalEle)
                )
            ] [
                Fa.span [
                    Fa.Solid.PlusCircle
                    Fa.FixedWidth ] [ ]
                str " Add User"
            ]
        ]
    ]

let displayAllUsersElement (model:Model) dispatch =
    div [Style [MarginBottom "5%"]][
        displayAllUsersNavbar model dispatch
        Column.column [
            Column.Width (Screen.All,Column.IsHalf);Column.Offset (Screen.All,Column.IsOneQuarter)
        ][
            Table.table [
                Table.IsFullWidth
            ] [
                thead [][
                    tr [][
                        th [][str "Username"]
                        th [][str "E-Mail"]
                        th [][str "Role"]
                        span [][]
                    ]
                ]
                tbody []
                    (model.AdminUserList
                    |> Array.filter (fun x -> if model.AdminUserListRoleFilter = All then x = x else x.Role = model.AdminUserListRoleFilter)
                    |> (Array.collect (fun userVal -> displayUser userVal dispatch)
                        >> List.ofArray)
                    )
            ]
        ]
    ]

type Props = {
    Model: Model
    Dispatch: Msg -> unit
}

let view = elmishView "UserList" <| fun { Model = model; Dispatch = dispatch } ->
    displayAllUsersElement model dispatch