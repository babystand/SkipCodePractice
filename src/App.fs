module Main
open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Fable.PowerPack.Fetch
open System
open System.Text.RegularExpressions
type QuestionState = | Unanswered | Correct | Incorrect
type QuestionModel = {
    Prompt : string
    Answer : string
    Input : string
    State : QuestionState
 }
type ScoreModel = {
    Score : int
    HighScore : int
 }
let initScore() = { Score = 0; HighScore = 0 }

type DictEntry = {
                  literal : string
                  skipCode : string
                  }

type Dictionary = {
                    size : int
                    kanji : DictEntry []
                  }
type LoadedState = {
    Dictionary : Dictionary
    Question : QuestionModel
    Score : ScoreModel
 }

type Model = | Unloaded | Loaded of LoadedState | Failed
type Msg =
    | FetchDictionary
    | FailDictionary of exn
    | LoadDictionary of Dictionary
    | FetchNewQuery
    | RestartTest
    | TryAnswer
    | SetInputValue of string


//----------

let init() = Unloaded, Cmd.ofMsg FetchDictionary

let fetchDictionary() : JS.Promise<Dictionary> =
    promise {
        let! res = fetch "/dict.json" []
        return! res.json<Dictionary>()
    }
//let clearInput() =
let validateInput str = Regex.Match(str, "[1-4]{1}[-]{1}[1-9]+[0-9]*[-]{1}[1-9]+[0-9]*").Success
 // UPDATE

let rec update (msg : Msg) (model : Model) =
    match msg with
    | FetchDictionary -> model, Cmd.ofPromise fetchDictionary () LoadDictionary FailDictionary
    | LoadDictionary d -> Loaded { Dictionary = d; Score = initScore(); Question = { Prompt = ""; Answer = ""; Input = ""; State = Unanswered } }, Cmd.ofMsg FetchNewQuery
    | FailDictionary e ->
        Browser.console.error "Couldn't load dictionary"
        Failed, Cmd.none
    | FetchNewQuery ->
            match model with
            | Unloaded | Failed -> model, Cmd.none
            | Loaded m ->
                       let rand : DictEntry = m.Dictionary.kanji.[Random().Next(m.Dictionary.size)]
                       (Loaded { m with Question = ({ Prompt = rand.literal; Answer = rand.skipCode; Input = ""; State = Unanswered }) }), Cmd.none
    | TryAnswer ->
                  match model with
                    | Unloaded | Failed -> model, Cmd.none
                    | Loaded m ->
                      if m.Question.State <> Unanswered then model, Cmd.ofMsg FetchNewQuery
                      else
                       if not <| validateInput m.Question.Input then Loaded { m with Score = { HighScore = Math.Max(m.Score.Score, m.Score.HighScore); Score = 0 }; Question = { m.Question with State = Incorrect } }, Cmd.none
                       else
                           if m.Question.Input = m.Question.Answer then Loaded { m with Score = { HighScore = Math.Max(m.Score.HighScore, m.Score.Score + 1); Score = m.Score.Score + 1 }; Question = { m.Question with State = Correct } }, Cmd.none
                           else Loaded { m with Score = { HighScore = Math.Max(m.Score.HighScore, m.Score.Score + 1); Score = 0 }; Question = { m.Question with State = Incorrect } }, Cmd.none
    | SetInputValue s ->
        match model with
        | Unloaded | Failed -> model, Cmd.none
                    | Loaded m -> Loaded { m with Question = { m.Question with Input = s } }, Cmd.none
    | RestartTest ->
        match model with
        | Unloaded | Failed -> model, Cmd.none
                    | Loaded m -> Loaded { m with Question = { Prompt = ""; Answer = ""; Input = ""; State = Unanswered }; Score = { m.Score with Score = 0 } }, Cmd.ofMsg FetchNewQuery

 // VIEW (rendered with React)

let view (model : Model) dispatch =
    match model with
    | Unloaded -> div [] []
    | Failed -> div [] []
    | Loaded m ->
    div [ Class "top-level" ] [
        div [ Class "container" ]
          [
            div [ Class "prompt-box" ] [
                    h1 [ Id "kanji-prompt" ] [ str m.Question.Prompt ]
                    br []
                    input [ Id "skip-code-answer"
                            Typeof "text"
                            Pattern "[1-4]{1}[-]{1}[1-9]+[0-9]*[-]{1}[1-9]+[0-9]*"
                            OnChange(fun ev -> ev.Value |> SetInputValue |> dispatch)
                            Value m.Question.Input
                           ]
                    br []

                    button [ Id "skip-code-submit"; OnClick(fun _ -> if m.Question.State = Unanswered then dispatch (TryAnswer) else dispatch FetchNewQuery) ] [ (if m.Question.State = Unanswered then (str "Answer") else (str "Next")) ]
                    button [ Id "skip-code-next"; OnClick(fun _ -> dispatch FetchNewQuery) ] [ str "Restart" ]
            ]
          ]
        div [ Class "score-box" ] [
            span [] [ str <| string m.Score.Score ]
            br []
            span [] [ str <| string m.Score.HighScore ]
        ]
      ]

 // App
Program.mkProgram init update view
 |> Program.withReact "elmish-app"
 |> Program.withConsoleTrace
 |> Program.run

//Queue loading

