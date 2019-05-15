module FetchHelpers

open Fetch
open Thoth.Json

let fetchWithDecoder<'T> (url: string) (decoder: Decoder<'T>) (init: RequestProperties list) =
    GlobalFetch.fetch(RequestInfo.Url url, Fetch.requestProps init)
    |> Promise.bind (fun response ->
        if not response.Ok then
            response.StatusText |> Error |> Promise.lift
        else
            response.text() |> Promise.map (Decode.fromString decoder))

// Inline the function so Fable can resolve the generic parameter at compile time
let inline fetchAs<'T> (url: string) (init: RequestProperties list) =
    // In this example we use Thoth.Json cached auto decoders
    // More info at: https://mangelmaxime.github.io/Thoth/json/v3.html#caching
    let decoder = Decode.Auto.generateDecoderCached<'T>()
    fetchWithDecoder url decoder init
