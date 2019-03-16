module FabulousExample.Main

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

let viewPicker items selectedItem onSelected =
    View.Picker
        (title = (if (Array.isEmpty items) then "Loading..." else "Select"),
         isEnabled = (not <| Array.isEmpty items),
         itemsSource = items,
         selectedIndex = (selectedItem |> Option.defaultValue -1),
         selectedIndexChanged = (fun (x, _) -> onSelected x))

let view (model : Model) dispatch =
    View.ContentPage(
        content = View.StackLayout(
            padding = 20.0, verticalOptions = LayoutOptions.Center, isEnabled = not model.isLoading,
            children = [ 
                viewPicker (model.countries |> Array.map (fun x -> x.Name)) model.selectedCountry (CountrySelected >> dispatch)
                viewPicker (model.states |> Array.map (fun x -> x.Region)) model.selectedState (StateSelected >> dispatch)
                viewPicker (model.cities |> Array.map (fun x -> x.City)) model.selectedCity (CitySelected >> dispatch)
            ]))

let program = Program.mkProgram init update view
