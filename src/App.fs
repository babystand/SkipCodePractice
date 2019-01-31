module Main
open Elmish
open Elmish.React
open Fable.Core
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Fable.PowerPack.Fetch
open System
open System.Text.RegularExpressions
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
              valid : bool
              dict : Dictionary
              }

type Msg =
 | FetchDictionary
 | FailDictionary of exn
 | LoadDictionary of Dictionary
 | FetchNewQuery
 | TryAnswer
 | SetInputValue of string
 | CheckValid

let init() = { prompt = ""; answer = ""; input = ""; valid = false; dict = { size = 0; kanji = [||] } }, Cmd.ofMsg FetchDictionary
let fetchDictionary() : JS.Promise<Dictionary> =
    promise {
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
    | TryAnswer -> model, Cmd.none
    | SetInputValue s -> { model with input = s }, Cmd.ofMsg CheckValid
    | CheckValid ->
        if Regex.Match(model.input, "[1-4]{1}[-]{1}[1-9]+[0-9]*[-]{1}[1-9]+[0-9]*").Success then
            { model with valid = true }, Cmd.none
        else
            model, Cmd.none

// VIEW (rendered with React)

let view (model : Model) dispatch =
    div [ Class "container" ]
      [
        div [ Class "prompt-box" ] [
                h2 [ Id "kanji-prompt" ] [ str model.prompt ]
                br []
                input [ Id "skip-code-answer"
                        Typeof "text"
                        Pattern "[1-4]{1}[-]{1}[1-9]+[0-9]*[-]{1}[1-9]+[0-9]*"
                        OnChange(fun ev -> ev.Value |> SetInputValue |> dispatch)
                       ]
                br []
                button [ Id "skip-code-submit"; OnClick(fun _ -> dispatch (TryAnswer)) ] [ str "Answer" ]
                button [ Id "skip-code-next"; OnClick(fun _ -> dispatch FetchNewQuery) ] [ str "Next" ]
        ]
      ]

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run

//Queue loading

