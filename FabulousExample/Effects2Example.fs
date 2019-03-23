module FabulousExample.Effects2Excample

open Fabulous.Core

let (>>-) a f =
    async {
        let! x = a
        return f x
    }

let mutable private globalRequest: Option<obj> = None
let mutable private globalResponse: Option<unit -> obj> = None

let runTest (f: 'a -> 'b) a =
    globalResponse <-
        Some(fun () ->
            let x = globalRequest.Value :?> 'a
            box <| f x)
    a |> Async.RunSynchronously

let wrap (x: 'a) (a: 'x Async) =
    async {
        if Option.isSome globalResponse
            then
                globalRequest <- Some <| box x
                let result = globalResponse.Value()

                globalRequest <- None
                globalResponse <- None

                return result :?> 'x
            else return! a
    }

let exampleAsync (x: int) =
    async {
        do! Async.Sleep 1000
        return sprintf "%i" x
    }

type ExampleAsyncEffect = ExampleAsyncEffect of int * (string -> unit)
let exampleAsync' (x: int) =
    exampleAsync x
    |> wrap (fun r -> ExampleAsyncEffect (x, r))

type Msg = Msg1 of string

let test() =
    let a = exampleAsync 42 >>- Msg1 |> Cmd.ofAsyncMsg
    let d = exampleAsync' 42 >>- Msg1
    let b = d |> Cmd.ofAsyncMsg

    let result =
        d |> runTest (
            fun (ExampleAsyncEffect (x, callback)) ->
                sprintf "%i" x)
    result = (Msg1 "42") |> ignore
    ()
