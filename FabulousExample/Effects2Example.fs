module FabulousExample.Effects2Example

open Fabulous.Core
open System
open System.Net

[<AutoOpen>]
module Utils =
    let inline curry f a b = f (a, b)
    let (>>-) a f =
        async {
            let! x = a
            return f x
        }
    let (>>-!) a f =
        async {
            let! x = a
            return Result.map f x
        }

module Cmd =
    let ofAsync a f =
        async {
            let! b = a |> Async.Catch
            return match b with | Choice1Of2 x -> f <| Ok x | Choice2Of2 e -> f <| Error e
        } |> Cmd.ofAsyncMsg
    let ofAsync0 a f =
        async {
            let! b = a
            return f b
        } |> Cmd.ofAsyncMsg

module Effect =
    let mutable private globalResponse : Option<obj -> unit> = None

    let runTest (f : 'eff -> unit) =
        globalResponse <-
            Some(fun eff ->
                    let x = eff :?> 'eff
                    f x)

    let wrap (fx : ('a -> unit) -> 'eff) (a : 'a Async) : 'a Async =
        async {
            if Option.isSome globalResponse
                then
                    let testFunc = globalResponse.Value

                    let mutable result : obj Option = None

                    let mutable effOpt : 'eff Option = None
                    effOpt <-
                        fx (fun x -> result <- Some <| box x)
                        |> Some

                    let eff = Option.get effOpt
                    testFunc eff

                    globalResponse <- None

                    return (Option.get result) :?> 'a
                else return! a
        }

    let private exampleAsync (x : int) =
        async {
            do! Async.Sleep 1000
            return sprintf "%i" x
        }

    type private ExampleAsyncEffect = ExampleAsyncEffect of int * (string -> unit)
    let private exampleAsync' (x : int) : string Async =
        exampleAsync x
        |> wrap (fun r -> ExampleAsyncEffect(x, r))

    type private Msg = Msg1 of string

    // let ``test #2``() =
    //     let a: Async<string> = downloadFromWeb (Uri "http://google.com/")

    //     let aResult: string =
    //         a
    //         |> runTest (fun (DownloadString(arg, callback)) -> callback "{}")

    //     let b: int Async = a >>- (fun x -> x.Length)

    //     let bResult: int =
    //         b
    //         |> runTest (fun (DownloadString(arg, callback)) -> callback "{}")

    //     ()

    // let private test() =
    //     let a : Cmd<Msg> = exampleAsync 42 >>- Msg1 |> Cmd.ofAsyncMsg
    //     let d : Async<Msg> = exampleAsync' 42 >>- Msg1
    //     let b : Cmd<Msg> = d |> Cmd.ofAsyncMsg

    //     let result : Msg =
    //         d
    //         |> runTest (
    //             fun (ExampleAsyncEffect(x, callback)) ->
    //                 sprintf "%i" x
    //                 |> callback)
    //     result = (Msg1 "42") |> ignore

module Effects =
    type DownloadString = DownloadString of Uri * (string -> unit)
    let downloadFromWeb (uri : Uri) =
        async { return! (new WebClient()).DownloadStringTaskAsync uri |> Async.AwaitTask }
        |> Effect.wrap (curry DownloadString uri)

module Services =
    open FSharp.Data

    type CountryProvider = JsonProvider<"data/countries.json">
    type StateProvider = JsonProvider<"data/states.json">
    type CityProvider = JsonProvider<"data/cities.json">

    let private key =
        System.Convert.FromBase64String "5dMq1voyzgIG+QH+BS7QATD/1P4r1vkDLwT+0//7M9M="
        |> (Array.scan (+) 127uy >> Array.map char >> System.String)

    let loadCountries =
        sprintf "https://battuta.medunes.net/api/country/all/?key=%s" key |> Uri
        |> Effects.downloadFromWeb
        >>- CountryProvider.Parse
    let loadStates country =
        sprintf "https://battuta.medunes.net/api/region/%s/all/?key=%s" country key |> Uri
        |> Effects.downloadFromWeb
        >>- StateProvider.Parse
    let loadCities country state =
        sprintf "https://battuta.medunes.net/api/city/%s/search/?region=%s&key=%s" country state key |> Uri
        |> Effects.downloadFromWeb
        >>- CityProvider.Parse

module Page =
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms

    type Model =
        { countries : Services.CountryProvider.Root array
          states : Services.StateProvider.Root array
          cities : Services.CityProvider.Root array
          isLoading : bool
          selectedCountry : int option
          selectedState : int option
          selectedCity : int option }

    type Target = | Country | State | City

    type Msg =
        | CountriesLoaded of Result<Services.CountryProvider.Root array, exn>
        | StatesLoaded of Result<Services.StateProvider.Root array, exn>
        | CitiesLoaded of Result<Services.CityProvider.Root array, exn>
        | ItemSelected of Target * int

    let initModel =
        { countries = [||]; states = [||]; cities = [||]
          selectedCountry = None; selectedState = None; selectedCity = None
          isLoading = true }

    let init() = initModel, Cmd.ofAsync Services.loadCountries CountriesLoaded

    let update msg model =
        match msg with
        | CountriesLoaded(Ok xs) -> { model with countries = xs; isLoading = false }, Cmd.none
        | StatesLoaded(Ok xs) -> { model with states = xs; isLoading = false }, Cmd.none
        | CitiesLoaded(Ok xs) -> { model with cities = xs; isLoading = false }, Cmd.none
        | ItemSelected(target, id) ->
            match target with
            | Country ->
                { model with selectedCountry = Some id
                             selectedState = None
                             selectedCity = None
                             isLoading = true
                             states = [||]
                             cities = [||] },
                Cmd.ofAsync (Services.loadStates (model.countries.[id].Code)) StatesLoaded
            | State ->
                { model with selectedState = Some id
                             selectedCity = None
                             isLoading = true
                             cities = [||] },
                Cmd.ofAsync
                    (Services.loadCities (model.countries.[model.selectedCountry.Value].Code) model.states.[id].Region)
                    CitiesLoaded
            | City -> { model with selectedCity = Some id }, Cmd.none
        | _ -> failwith "TODO"

    let viewPicker items map selectedItem onSelected =
        View.Picker (
            title = (if (Array.isEmpty items) then "Loading..." else "Select"),
            isEnabled = (not <| Array.isEmpty items),
            itemsSource = (items |> Array.map map),
            selectedIndex = (selectedItem |> Option.defaultValue -1),
            selectedIndexChanged = (fun (x, _) -> onSelected x))

    let view model dispatch =
        View.ContentPage (
            content = View.StackLayout (
                padding = 20.0, verticalOptions = LayoutOptions.Center,
                isEnabled = not model.isLoading,
                children = [
                    viewPicker model.countries (fun x -> x.Name) model.selectedCountry (curry ItemSelected Country >> dispatch)
                    viewPicker model.states (fun x -> x.Region) model.selectedState (curry ItemSelected State >> dispatch)
                    viewPicker model.cities (fun x -> x.City) model.selectedCity (curry ItemSelected City >> dispatch) ]))

module PageTest =
    let test() =

        let (model, cmd) = Page.init()

        printfn "Assert model %O" model

        Effect.runTest (
            fun (Effects.DownloadString(arg, f)) ->
                f <| sprintf "Mock string, for url %O" arg
            )

        Cmd.dispatch
            (fun newMsg -> printfn "Assert new msg %O" newMsg)
            cmd
