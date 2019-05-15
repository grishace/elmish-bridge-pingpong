open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Elmish.Bridge
open Shared
open SocketServer

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let initState() : Task<PingPong> = Task.FromResult Ping

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let! res = initState()
            return! json res next ctx
        })
    forward "/socket" socketServer
}

let app = application {
    url ("http://*:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
    app_config Giraffe.useWebSockets
}

run app
