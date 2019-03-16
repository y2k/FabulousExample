module FabulousExample.Main

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

[<AutoOpen>]
module Operators =
    let inline (>>-) a f = async { let! x = a
                                   return f x }

module Services =
    open FSharp.Data

    type CountryProvider = JsonProvider<"data/countries.json">
    type StateProvider = JsonProvider<"data/states.json">
    type CityProvider = JsonProvider<"data/cities.json">

    let private key =
        System.Convert.FromBase64String "5dMq1voyzgIG+QH+BS7QATD/1P4r1vkDLwT+0//7M9M="
        |> fun x -> System.String(Array.map char (Array.scan (+) 127uy x))

    let loadCountries =
        sprintf "https://battuta.medunes.net/api/country/all/?key=%s" key 
        |> CountryProvider.AsyncLoad
    let loadStates country =
        sprintf "https://battuta.medunes.net/api/region/%s/all/?key=%s" country key 
        |> StateProvider.AsyncLoad
    let loadCities country region =
        sprintf "https://battuta.medunes.net/api/city/%s/search/?region=%s&key=%s" country region key
        |> CityProvider.AsyncLoad

type Model =
    { countries : Services.CountryProvider.Root array
      states : Services.StateProvider.Root array
      cities : Services.CityProvider.Root array
      isLoading : bool
      selectedCountry : int option
      selectedState : int option
      selectedCity : int option }

type Msg =
    | CountriesLoaded of Services.CountryProvider.Root array
    | CountrySelected of int
    | StatesLoaded of Services.StateProvider.Root array
    | StateSelected of int
    | CitiesLoaded of Services.CityProvider.Root array
    | CitySelected of int

let initModel =
    { countries = [||]
      states = [||]
      cities = [||]
      isLoading = true
      selectedCountry = None
      selectedState = None
      selectedCity = None }

let init() = initModel, Cmd.ofAsyncMsg <| (Services.loadCountries >>- CountriesLoaded)

let update msg model =
    match msg with
    | CountriesLoaded xs -> { model with countries = xs; isLoading = false }, Cmd.none
    | CountrySelected x ->
        { model with selectedCountry = Some x
                     selectedState = None
                     selectedCity = None
                     isLoading = true
                     states = [||]
                     cities = [||] },
        Cmd.ofAsyncMsg (Services.loadStates (model.countries.[x].Code.String.Value) >>- StatesLoaded)
    | StatesLoaded xs -> { model with states = xs; isLoading = false }, Cmd.none
    | StateSelected id ->
        { model with selectedState = Some id
                     selectedCity = None
                     isLoading = true
                     cities = [||] },
        Cmd.ofAsyncMsg
            (Services.loadCities 
                (model.countries.[model.selectedCountry.Value].Code.String.Value)
                model.states.[id].Region
             >>- CitiesLoaded)
    | CitiesLoaded xs -> { model with cities = xs; isLoading = false }, Cmd.none
    | CitySelected id -> { model with selectedCity = Some id }, Cmd.none

let view (model : Model) dispatch =
    View.ContentPage
        (content = View.StackLayout
                       (padding = 20.0, verticalOptions = LayoutOptions.Center, isEnabled = not model.isLoading,
                        children = [ View.Picker
                                         (title = (if model.countries.Length = 0 then "Loading..."
                                                   else "Select country"),
                                          isEnabled = (not <| Array.isEmpty model.countries),
                                          itemsSource = (model.countries |> Array.map (fun x -> x.Name)),
                                          selectedIndex = (model.selectedCountry |> Option.defaultValue -1),
                                          selectedIndexChanged = (fun (x, _) -> dispatch <| CountrySelected x))

                                     View.Picker
                                         (title = (if model.states.Length = 0 then "Loading..."
                                                   else "Select region"),
                                          isEnabled = (not <| Array.isEmpty model.states),
                                          itemsSource = (model.states |> Array.map (fun x -> x.Region)),
                                          selectedIndex = (model.selectedCountry |> Option.defaultValue -1),
                                          selectedIndexChanged = (fun (x, _) -> dispatch <| StateSelected x))

                                     View.Picker
                                         (title = (if model.cities.Length = 0 then "Loading..."
                                                   else "Select city"),
                                          isEnabled = (not <| Array.isEmpty model.cities),
                                          itemsSource = (model.cities |> Array.map (fun x -> x.City)),
                                          selectedIndex = (model.selectedCountry |> Option.defaultValue -1),
                                          selectedIndexChanged = (fun (x, _) -> dispatch <| CitySelected x)) ]))

// Note, this declaration is needed if you enable LiveUpdate
let program = Program.mkProgram init update view
