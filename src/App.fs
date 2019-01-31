module Main
open Elmish
open Elmish.React
open Fable.Core
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open System
open Fable.PowerPack
open Fable.PowerPack.Fetch
// MODEL
type DictEntry = {
                  literal : string
                  skipCode : string
                  }

type Dictionary = {
                    size : int
                    kanji : DictEntry []
                  }
type Model = {
              prompt : string
              answer : string
              input : string
              dict : Dictionary
              }

type Msg =
 | FetchDictionary
 | FailDictionary of exn
 | LoadDictionary of Dictionary
 | FetchNewQuery
 | TryAnswer of string
 | FormatAnswer of string

let init() = { prompt = ""; answer = ""; input = ""; dict = { size = 0; kanji = [||] } }, Cmd.ofMsg FetchDictionary
let fetchDictionary () : JS.Promise<Dictionary> =
    promise{
        let! res = fetch "/dict.json" []
        return! res.json<Dictionary>()
    }

// UPDATE

let rec update (msg : Msg) (model : Model) =
    match msg with
    | FetchDictionary -> model, Cmd.ofPromise fetchDictionary () LoadDictionary FailDictionary
    | LoadDictionary d -> { model with dict = d }, Cmd.ofMsg FetchNewQuery
    | FailDictionary e ->
        Browser.console.error "Couldn't load dictionary"
        model, Cmd.none
    | FetchNewQuery ->
            let rand : DictEntry = model.dict.kanji.[Random().Next(model.dict.size)]
            { model with prompt = rand.literal; answer = rand.skipCode; input = "" }, Cmd.none
    | TryAnswer s -> model, Cmd.none
    | FormatAnswer s -> model, Cmd.none

// VIEW (rendered with React)

let view (model : Model) dispatch =
    div []
      [
        h2 [] [ str model.prompt ]
        br []
        input []
        br []
        button [ OnClick(fun _ -> dispatch (TryAnswer model.input)) ] [ str "Answer" ]
      ]

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run

//Queue loading

