namespace FabulousExample

[<AutoOpen>]
module Utils =
    let inline (>>-) a f = async { let! x = a
                                   return f x }
    let inline curry f a b = f (a, b)

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
    let loadCities country state =
        sprintf "https://battuta.medunes.net/api/city/%s/search/?region=%s&key=%s" country state key
        |> CityProvider.AsyncLoad

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
        | CountriesLoaded of Services.CountryProvider.Root array
        | ItemSelected of Target * int
        | StatesLoaded of Services.StateProvider.Root array
        | CitiesLoaded of Services.CityProvider.Root array

    let initModel =
        { countries = [||]; states = [||]; cities = [||]
          selectedCountry = None; selectedState = None; selectedCity = None 
          isLoading = true }

    let init() = initModel, Cmd.ofAsyncMsg <| (Services.loadCountries >>- CountriesLoaded)

    let update msg model =
        match msg with
        | CountriesLoaded xs -> { model with countries = xs; isLoading = false }, Cmd.none
        | StatesLoaded xs -> { model with states = xs; isLoading = false }, Cmd.none
        | CitiesLoaded xs -> { model with cities = xs; isLoading = false }, Cmd.none
        | ItemSelected (target, id) ->
            match target with
            | Country -> 
                { model with selectedCountry = Some id
                             selectedState = None
                             selectedCity = None
                             isLoading = true
                             states = [||]
                             cities = [||] },
                Cmd.ofAsyncMsg (Services.loadStates (model.countries.[id].Code) >>- StatesLoaded)
            | State -> 
                { model with selectedState = Some id
                             selectedCity = None
                             isLoading = true
                             cities = [||] },
                Cmd.ofAsyncMsg
                    (Services.loadCities (model.countries.[model.selectedCountry.Value].Code) model.states.[id].Region
                     >>- CitiesLoaded)
            | City -> { model with selectedCity = Some id }, Cmd.none

    let viewPicker items map selectedItem onSelected =
        View.Picker
            (title = (if (Array.isEmpty items) then "Loading..." else "Select"),
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
