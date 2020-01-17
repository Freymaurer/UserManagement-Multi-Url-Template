module Client.App

open Elmish
open Elmish.UrlParser
open Elmish.Navigation
open Thoth.Json
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fulma

open Client.AuxFunctions
open Client.View
open Client.Types
open Client.State

let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
             str ", "
             a [ Href "https://zaid-ajaj.github.io/Fable.Remoting/" ] [ str "Fable.Remoting" ]
           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let view (model:Model) (dispatch: Msg -> unit) =
    div [ ] [
        // side menu coming in from the left
        UserManagement.View.SidebarMenu.menuCard model dispatch
        // top navbar
        Navbar.navbar [ Navbar.Color IsWhiteBis; Navbar.Props [Style [BorderBottom "1px solid grey"]] ]
            (if model.Authenticated = true
                then (UserManagement.View.NavbarLogin.loggedInNavbar model dispatch)
                else (UserManagement.View.NavbarLogin.loginNavbar model dispatch )
            )
        //(heroHeadNavbar dispatch model)
        (extraEle model dispatch)
        div [ ] [
            match model.CurrentPageModel, model.CurrentRoute with
            //| RootModel, _ ->
            //    yield Home.view {Model = {Placeholder = None}; Dispatch = (HomeMsg >> dispatch)}
            | HomeModel m, Some Route.Home ->
                yield Home.view {Model = m; Dispatch = (HomeMsg >> dispatch)}
            | CounterModel m ,Some Route.Counter->
                yield Counter.view { Model = m; Dispatch = (CounterMsg >> dispatch) }
            | UserAccountModel m, Some (Route.UserAccount _) ->
                yield UserAccount.view { Model = m; Dispatch = (UserAccountMsg >> dispatch)}
            | UserListModel m, Some (Route.UserList) ->
                yield UserList.view { Model = m; Dispatch = (UserListMsg >> dispatch)}
                //yield str "this does not exist yet"
            | _, Some (Route.Detail id) ->
                yield str (sprintf "you just matched the Detail Route with id: %i" id)
            | _ ->
                yield str "this does not exist yet"
        ]
        Hero.foot [] [
            Content.content [
                Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
                Content.Props [Style [ MarginTop "1rem" ] ]
            ] [ safeComponents ]
        ]
    ]

open Elmish.React
open Elmish.Debug
open Elmish.Navigation
open Elmish.UrlParser
open Elmish.Navigation

let navigable = Program.toNavigable Routing.parsePath urlUpdate

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withDebugger
#endif
#if DEBUG
|> Program.withConsoleTrace
#endif
|> navigable //(Browser.Types.Location -> Route option) -> (Route option -> Model -> Model * Cmd<Msg>)
|> Program.withReactBatched "elmish-app"
|> Program.run