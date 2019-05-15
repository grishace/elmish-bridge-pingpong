module Shared

type PingPong = Ping | Pong

type Msg =
| InitialStateLoaded of Result<PingPong, string>
| ConnectSocket
| NewState of PingPong

type RemoteServerMessage =
| Connect
| Action of PingPong

type ServerMsg =
  | RS of RemoteServerMessage
  | Closed
