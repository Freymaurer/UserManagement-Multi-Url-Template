module Client.UserManagement.View.SidebarMenu

open Fable.React
open Fable.React.Props
open Fulma
open Shared
open Fable.FontAwesome

open Client.Types

// Helper to generate a menu item
let menuItem label msg =
    Menu.Item.li [ Menu.Item.OnClick msg]
        [ str label ]

let redirectMenuItem label route dispatch=
    Menu.Item.li [
        Menu.Item.Props [
            Href "#";
            OnClick (
                fun ev ->
                    ev.preventDefault();
                    Msg.ToggleMenu |> dispatch ;
                    Msg.Navigate route |> dispatch
            )
        ]
    ] [str label]

// Helper to generate a sub menu
let subMenu label children =
    li [ ] [
        Menu.Item.a [ ]
           [ str label ]
        ul [ ] children
    ]

let redirectLinks model dispatch =
    let detailNumberInputField =
        Input.number [
            Input.Placeholder "Detail Number..."
            Input.OnChange (fun e ->
                let x = e.Value
                dispatch (UpdateNumberForDetail (int x))
            )
        ]
    let account =
        if model.User.IsSome then model.User.Value.UniqueId else "No-Access"

    div [] [
        Menu.list [ (*Tabs.IsBoxed; Tabs.IsCentered; Tabs.Props [Style [BorderBottom "1px solid grey"]]*) ] [
            redirectMenuItem "Home" Route.Home dispatch
            redirectMenuItem "Counter" Route.Counter dispatch
            Menu.label [ ] [ str "User Account" ]
            redirectMenuItem "Account" (Route.UserAccount ("user",account)) dispatch
            Menu.label [ ] [ str "Admin Account Management" ]
            redirectMenuItem "User List" Route.UserList dispatch
            redirectMenuItem (sprintf "Detail %i" model.NumberForDetail) (Route.Detail model.NumberForDetail) dispatch
            Menu.Item.li [] [detailNumberInputField]
        ]
    ]

let menu (model:Model) dispatch =
    let hideElementsBy threshold=
        Screen.All, if AuxFunctions.authentificationLevelByUser model.User >= threshold then false else true
    let unAuthenticated =
        div [
            Style [BackgroundColor "white";PaddingTop "1rem";PaddingLeft "1rem";Height "100%";]
        ][
            str "Not logged in."
        ]
    //let authenticated =
    //    Menu.menu [
    //        Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is7)]
    //        Props [Style [BackgroundColor "white";PaddingTop "1rem";PaddingLeft "1rem";Height "100%"]]
    //    ] [
    //        Menu.list [ ]
    //            [  ]
    //        Menu.label [ ] [ str "User Account" ]
    //        Menu.list [ ] [
    //            menuItem "Home" (fun _ -> dispatch (ChangeMainReactElement Welcome))
    //            menuItem "Account Information" (fun _ -> dispatch (ChangeMainReactElement (UserAccount model.User.Value))) ]
    //        Menu.label [ Modifiers [Modifier.IsHidden (hideElementsBy 5)] ] [ str "Account Management" ]
    //        Menu.list [ Modifiers [Modifier.IsHidden (hideElementsBy 5)] ] [
    //            menuItem "User List"  (fun _ ->
    //            dispatch AdminGetAllUsersRequest
    //            dispatch (ChangeMainReactElement UserList))
    //        ]
    //        Menu.label [ Modifiers [Modifier.IsHidden (hideElementsBy 10)] ] [str "Debug"]
    //        Menu.list [Modifiers [Modifier.IsHidden (hideElementsBy 10)]]
    //            [menuItem "Test Counter" (fun _ -> dispatch (ChangeMainReactElement Counter))]
    //    ]
    Menu.menu [
        Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is7)]
        Props [Style [BackgroundColor "white";PaddingTop "1rem";PaddingLeft "1rem";Height "100%"]]
    ] [
        match model.Authenticated with
        | true ->
            (redirectLinks model dispatch); (*authenticated*)
        | false -> (redirectLinks model dispatch); unAuthenticated
    ]

let menuCard model dispatch =
    div [
        Style [
            Position PositionOptions.Absolute;
            BackgroundColor "rgba(0, 0, 0, 0.5)"
            ZIndex "100"
            Width "100%"
            Height "100%"
            Transition "Visibility"
            TransitionDuration (if model.ShowMenuBool = true then ".20s" else ".50s")
            Visibility (if model.ShowMenuBool = true then "visible" else "hidden")
        ]
    ][
        /// just a background element that will toggle off the menu when clicked on
        div [OnClick (fun _ -> dispatch ToggleMenu);Style [Width "100%";Height "100%";Position PositionOptions.Absolute;ZIndex "200"]][]
        /// The menu itself
        Column.column [
            Column.Width (Screen.All,Column.Is2);
            Column.Modifiers [ Modifier.IsPaddingless ];
            Column.Props [
                Style [
                    Height "100%";ZIndex "300"; Position PositionOptions.Absolute
                    Transform (if model.ShowMenuBool = true then "translate3d(0, 0, 0)" else "translate3d(-100%, 0, 0)")
                    TransitionDuration ".50s"
                    TransitionProperty "transform"
                ]
            ]
        ] [
            Navbar.navbar [ Navbar.Props [Style [BackgroundColor "black"]] ] [
                Navbar.Item.div [Navbar.Item.Props [ Style [ MarginLeft "1rem"; MarginRight "0.5rem" ;MinHeight "3.25rem"] ]][
                    Fa.i [
                        Fa.Solid.Bars
                        Fa.Props [
                            OnClick (fun _ ->  dispatch ToggleMenu)
                            Style [
                                Cursor "pointer"
                                PaddingTop "0.5rem"
                            ]
                        ]
                    ] [ ]
                ]
                Navbar.Item.div [Navbar.Item.Props [ Style [ MarginRight "0.5rem" ;MinHeight "3.25rem"; PaddingBottom "0"; Width "60%"] ]][
                    text [Style[Color "#e6e6e6"; FontSize "smaller"] ][str (if model.User.IsSome then model.User.Value.Username else "Log In")]
                ]
            ]
            menu model dispatch
        ]
    ]