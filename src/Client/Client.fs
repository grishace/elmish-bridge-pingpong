module Client

open System
open Fable.Core
open Elmish
open Elmish.Bridge
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma

open FetchHelpers
open Shared

let id = Guid.NewGuid()

type Model = {
    State: PingPong option
    SocketConnected: bool
}

let initialState () = fetchAs<PingPong> "/api/init" []

let init () : Model * Cmd<Msg> =
    let initialModel = { State = None; SocketConnected = false }
    let loadStateCmd =
        Cmd.OfPromise.either
            initialState
            ()
            InitialStateLoaded
            (fun ex -> ex.Message |> Error |> InitialStateLoaded)
    initialModel, loadStateCmd

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.State, msg with
    | _, InitialStateLoaded (Ok initialState)->
        let nextModel = { State = Some initialState; SocketConnected = false }
        nextModel, Cmd.ofMsg ConnectSocket

    | _, ConnectSocket ->
        try
            Bridge.Send Connect
            { currentModel with SocketConnected = true }, Cmd.none
        with _ ->
            let delay () = promise {
                do! Async.Sleep 1000 |> Async.StartAsPromise
            }
            let checkSocket = (fun _ -> ConnectSocket)
            { currentModel with SocketConnected = false }, Cmd.OfPromise.either delay () checkSocket checkSocket

    | _, NewState Ping ->
        let nextModel = { currentModel with State = Some Ping }
        nextModel, Cmd.none

    | _, NewState Pong ->
        let nextModel = { currentModel with State = Some Pong }
        nextModel, Cmd.none

    | _ -> currentModel, Cmd.none

let button dis txt onClick =
    Button.button
        [ Button.Disabled dis
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let show = function
| { SocketConnected = connected; State = Some state } ->
    button (not connected) (string state) (fun _ -> Bridge.Send (Action state))
| { State = None   } -> str "Loading..."

let view (model : Model) (dispatch : Msg -> unit) =
    div [ Style [ Padding 50.0 ] ] [

        ol [ Style [ MarginBottom 50.0 ]  ] [
            li [] [ str "Open developer tools" ]
            li [] [ str "Refresh the page" ]
            li [] [ str "Select Network tab, socket URL in the requests list, and Messages preview in the right pane" ]
            li [] [ str "Watch Elmish.Bridge messages"]
        ]

        show model ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withBridgeConfig (Bridge.endpoint "./socket" |> Bridge.withUrlMode Append)
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "ping-pong"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
