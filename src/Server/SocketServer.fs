module SocketServer

open Giraffe
open Elmish
open Elmish.Bridge
open Shared

type ConnectionState =
  | Connected
  | Disconnected

let connections = ServerHub<ConnectionState, ServerMsg, Msg>().RegisterServer(RS)

let update clientDispatch msg state =
    match msg with
    | RS msg ->
        match msg with
        | Connect ->
            Connected, Cmd.none
        | Action Ping ->
            NewState Pong |> clientDispatch
            state, Cmd.none
        | Action Pong ->
            NewState Ping |> clientDispatch
            state, Cmd.none
    | _ -> Disconnected, Cmd.none

let init _ () = Disconnected, Cmd.none

let socketServer : HttpHandler =
    Bridge.mkServer "" init update
    |> Bridge.register RS
    |> Bridge.whenDown Closed
    |> Bridge.withServerHub connections
    |> Bridge.run Giraffe.server
