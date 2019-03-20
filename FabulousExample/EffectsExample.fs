module FabulousExample.EffectsExample

[<AutoOpen>]
module Utils =
    let inline (>>-) a f = async { let! x = a
                                   return f x }
    let inline curry f a b = f (a, b)

    type Effect<'a> = obj * (obj -> 'a) * Async<Result<'a, exn>>

    let (<*>) (e : 'e, f : obj -> 'b, a : Async<Result<'b, exn>>) (g : 'b -> 'c) : Effect<'c> = 
        box e, f >> g, async { let! x = a
                               return Result.map g x }
    let (<*!>) (e : 'e, f : obj -> 'b, a : Async<Result<'b, exn>>) (g : Result<'b, exn> -> 'c) : Effect<'c> = 
        box e, f >> Ok >> g, async { let! x = a
                                     return Ok <| g x }

module Effects =
    open System
    let private catch a = async { let! x = a |> Async.Catch
                                  return x |> function Choice1Of2 x -> Ok x | Choice2Of2 x -> Error x }

    type DownloadFromWeb = DownloadFromWeb of Uri
    let downloadFromWeb (uri : Uri) : Effect<string> =
        box <| DownloadFromWeb uri,
        (fun x -> box x :?> string),
        async { return! (new Net.WebClient()).DownloadStringTaskAsync uri |> Async.AwaitTask |> catch }

    let none<'a> : Effect<'a> =
        box (), (fun _ -> invalidOp ""), async.Return <| Error (Exception())

module Services =
    open System
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
        <*> CountryProvider.Parse
    let loadStates country =
        sprintf "https://battuta.medunes.net/api/region/%s/all/?key=%s" country key |> Uri
        |> Effects.downloadFromWeb
        <*> StateProvider.Parse
    let loadCities country state =
        sprintf "https://battuta.medunes.net/api/city/%s/search/?region=%s&key=%s" country state key |> Uri
        |> Effects.downloadFromWeb
        <*> CityProvider.Parse

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

    type Target = Country | State | City

    type Msg =
        | CountriesLoaded of Result<Services.CountryProvider.Root array, exn>
        | StatesLoaded of Result<Services.StateProvider.Root array, exn>
        | CitiesLoaded of Result<Services.CityProvider.Root array, exn>
        | ItemSelected of Target * int

    let initModel =
        { countries = [||]; states = [||]; cities = [||]
          selectedCountry = None; selectedState = None; selectedCity = None 
          isLoading = true }

    let init () = initModel, Services.loadCountries <*!> CountriesLoaded

    let update msg model =
        match msg with
        | CountriesLoaded (Ok xs) -> { model with countries = xs; isLoading = false }, Effects.none
        | StatesLoaded (Ok xs) -> { model with states = xs; isLoading = false }, Effects.none
        | CitiesLoaded (Ok xs) -> { model with cities = xs; isLoading = false }, Effects.none
        | ItemSelected (target, id) ->
            match target with
            | Country -> 
                { model with selectedCountry = Some id
                             selectedState = None
                             selectedCity = None
                             isLoading = true
                             states = [||]
                             cities = [||] },
                Services.loadStates (model.countries.[id].Code) <*!> StatesLoaded
            | State -> 
                { model with selectedState = Some id
                             selectedCity = None
                             isLoading = true
                             cities = [||] },
                Services.loadCities (model.countries.[model.selectedCountry.Value].Code) model.states.[id].Region
                <*!> CitiesLoaded
            | City -> { model with selectedCity = Some id }, Effects.none
        | _ -> failwith "TODO"

    let viewPicker items map selectedItem onSelected =
        View.Picker(
            title = (if (Array.isEmpty items) then "Loading..." else "Select"),
            isEnabled = (not <| Array.isEmpty items),
            itemsSource = (items |> Array.map map),
            selectedIndex = (selectedItem |> Option.defaultValue -1),
            selectedIndexChanged = (fun (x, _) -> onSelected x))

    let view model dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = 20.0, verticalOptions = LayoutOptions.Center, 
                isEnabled = not model.isLoading,
                children = [ 
                    viewPicker model.countries (fun x -> x.Name) model.selectedCountry (curry ItemSelected Country >> dispatch)
                    viewPicker model.states (fun x -> x.Region) model.selectedState (curry ItemSelected State >> dispatch)
                    viewPicker model.cities (fun x -> x.City) model.selectedCity (curry ItemSelected City >> dispatch) ]))

module Example4 =
    open System
    open Fabulous.Core

    type Effect<'a> = obj * (obj -> 'a) * Async<Result<'a, exn>>

    let map (g : 'b -> 'c) (e : 'e, f : obj -> 'b, a : Async<Result<'b, exn>>) : Effect<'c> = 
        box e, f >> g, async { let! x = a
                               return Result.map g x }
    let map' (g : Result<'b, exn> -> 'c) (e : 'e, f : obj -> 'b, a : Async<Result<'b, exn>>) : Effect<'c> = 
        box e, f >> Ok >> g, async { let! x = a
                                     return Ok <| g x }

    module Effects =
        let private catch a = async { let! x = a |> Async.Catch
                                      return x |> function Choice1Of2 x -> Ok x | Choice2Of2 x -> Error x }

        type DownloadFromWeb = DownloadFromWeb of Uri
        let downloadFromWeb (uri : Uri) : Effect<string> =
            box <| DownloadFromWeb uri,
            (fun x -> box x :?> string),
            async { return! (new Net.WebClient()).DownloadStringTaskAsync uri |> Async.AwaitTask |> catch }

        type MakeRandomEff = MakeRandomEff of min:int * max:int
        let makeRandomEff min max : Effect<int> = 
            box <| MakeRandomEff (min, max),
            (fun x -> box x :?> int),
            async { return (Random().Next(min,max)) } |> catch
    
    module Domain = 
        type Type1 = Type1
        let foo (_ : string) : Type1 = failwith ""

    type Msg = Msg0 | Msg1 of Result<Domain.Type1, exn> | Msg2 of Result<string, exn> | Msg3 of Result<int, exn>
    type Model = Model

    let update (model : Model) (msg : Msg) =
        match msg with
        | Msg0 _ -> model, Effects.downloadFromWeb (Uri "") |> map Domain.foo |> map' Msg1
        | Msg1 _ -> model, Effects.downloadFromWeb (Uri "") |> map' Msg2
        | Msg2 _ -> model, Effects.makeRandomEff 0 100 |> map' Msg3 
        | Msg3 _ -> model, failwith "???"

    let inline toCmdUpdate (update : 'model -> 'msg -> 'model * 'msg Effect) model msg =
        update model msg |> (fun (_, (_, _, a)) -> Cmd.ofAsyncMsg a)

    let update' (model : Model) (msg : Msg) = 
        update model msg

    let ``test Msg0`` () =
        let (model, (e, f, _)) = update Model (Msg2 <| Ok "")
        printf "assert model %O" (model = Model)

        let e' = e :?> Effects.DownloadFromWeb
        printfn "assert effect %O" (e' = Effects.DownloadFromWeb (Uri ""))

        let f' = match f "{}" with Msg1 x -> x | _ -> failwith ""
        printfn "assert Msg1 argument %O" (f' = Ok Domain.Type1)
