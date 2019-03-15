namespace FabulousExample

open System.Diagnostics
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

module Services =

    let loadCountries = 
        async {
            do! Async.Sleep 1500
            return [ "Russia"; "USA"; "Germany" ]
        }

module App = 
    type Model = 
      { countries: string list
        selectedCountry : int option }

    type Msg = 
        | SelectCountryChanged of int
        | CountriesLoaded of string list

    let initModel = { countries = []; selectedCountry = None }

    let init () = initModel, Cmd.ofAsyncMsg <| async { let! xs = Services.loadCountries; 
                                                       return CountriesLoaded xs }

    let update msg model =
        match msg with
        | SelectCountryChanged x -> { model with selectedCountry = Some x }, Cmd.none
        | CountriesLoaded xs -> { model with countries = xs }, Cmd.none

    let view (model: Model) dispatch =
        View.ContentPage(
          content = View.StackLayout(padding = 20.0, verticalOptions = LayoutOptions.Center,
            children = [ 

                View.Picker(title = (if model.countries.Length = 0 then "Loading..." else "Select country"),
                            itemsSource = model.countries, 
                            selectedIndex = (model.selectedCountry |> Option.defaultValue -1),
                            selectedIndexChanged = (fun (x,_) -> dispatch <| SelectCountryChanged x))

                View.Picker(title = (if model.countries.Length = 0 then "Loading..." else "Select region"),
                            itemsSource = model.countries, 
                            selectedIndex = (model.selectedCountry |> Option.defaultValue -1),
                            selectedIndexChanged = (fun (x,_) -> dispatch <| SelectCountryChanged x))

                View.Picker(title = (if model.countries.Length = 0 then "Loading..." else "Select city"),
                            itemsSource = model.countries, 
                            selectedIndex = (model.selectedCountry |> Option.defaultValue -1),
                            selectedIndexChanged = (fun (x,_) -> dispatch <| SelectCountryChanged x))

            ]))

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/models.html for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif
