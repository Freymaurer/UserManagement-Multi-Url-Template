module Client.Counter

open Fable.React
open Fable.React.Props
open Fulma
open Elmish
open Fable.React
open Fulma

open Shared

open Client.AuxFunctions
open Client.View

type ExtraReactElement =
|EmptyElement
|Message of string

and Model = {
    Counter: Counter option
    Loading: bool
    ErrorMsg: string option
    ExtraReactElement : ExtraReactElement
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| Increment
| Decrement
| InitialCountLoaded of Counter
| GetUserCounterRequest
| GetUserCounterResponse of Result<Counter,exn>
| UpdateExtraElement of ExtraReactElement

module Server =

    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IUserApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder ServerPath.normalizeRoutes
        |> Remoting.buildProxy<IUserApi>

    let dotnetSecureApi : IDotnetSecureApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder ServerPath.normalizeRoutes
        |> Remoting.buildProxy<IDotnetSecureApi>

let initialCounter = Server.api.initialCounter

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Counter = None; Loading = true; ErrorMsg = None; ExtraReactElement = EmptyElement }
    let loadCountCmd =
        Cmd.OfAsync.perform initialCounter () InitialCountLoaded
    initialModel, loadCountCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some counter, Increment ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
        nextModel, Cmd.none
    | _, InitialCountLoaded initialCount->
        let nextModel = { Counter = Some initialCount; Loading = false; ErrorMsg = None; ExtraReactElement = EmptyElement }
        nextModel, Cmd.none
    | _ , GetUserCounterRequest ->
        let nextModel = { currentModel with Loading = true }
        let cmd =
            Cmd.OfAsync.either
                Server.dotnetSecureApi.getUserCounter
                ()
                (Ok >> GetUserCounterResponse)
                (Error >> GetUserCounterResponse)
        nextModel, cmd
        ///
    | _ , GetUserCounterResponse (Ok value)->
        let nextModel = { currentModel with Loading = false; Counter = Some value }
        nextModel, Cmd.none
        ///
    | _ , GetUserCounterResponse (Error e)->
        let nextModel = {
            currentModel with
                Loading = false
                ErrorMsg = Some e.Message
                ExtraReactElement = Message "This function is for User only"
        }
        nextModel, Cmd.none
        ///
    | _ , UpdateExtraElement (element) ->
        let nextModel = { currentModel with ExtraReactElement = element }
        nextModel, Cmd.none
        ///
    | _ -> currentModel, Cmd.none

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let show model =
    match model with
    | { Counter = Some counter; Loading = false } -> string counter.Value
    | _ -> "Loading..."

let extraEle model dispatch =
    match model.ExtraReactElement with
    | EmptyElement -> Helper.emptyStr
    | Message x -> Helper.messageContainer x (fun _ -> dispatch (UpdateExtraElement EmptyElement))

let counter model dispatch =
      Container.container [ Container.Props [ Style [MarginTop "1rem"] ] ] [
        Column.column [Column.Modifiers [Modifier.TextAlignment (Screen.All,TextAlignment.Centered)]][
            Heading.h6 [] [ str "Welcome! This is currently a placeholder Welcome-Screen. Please login to access user management functions." ]
        ]
        (extraEle model dispatch)
        Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
           [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
        Columns.columns []
           [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
             Column.column [] [ button "+" (fun _ -> dispatch Increment) ]
             Column.column [] [ button "secret" (fun _ -> dispatch GetUserCounterRequest) ] ] ]

type Props = {
    Model: Model
    Dispatch: Msg -> unit
}

let view = elmishView "Counter" <| fun { Model = model; Dispatch = dispatch } ->
    counter model dispatch