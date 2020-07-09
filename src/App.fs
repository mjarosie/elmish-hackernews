module App

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp
open Thoth.Json
open Fable.DateFunctions
open System
open Fable.Core.JsInterop

type Product = { name: string; price: float }

type StoreInfo =
    { name: string
      since: string
      daysOpen: string list
      products: Product list }

module Cmd =
    let fromAsync (operation: Async<'msg>): Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit): unit =
            let delayedDispatch =
                async {
                    let! msg = operation
                    dispatch msg
                }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd

module Async =
    let map (f: 'T -> 'U) (op: Async<'T>): Async<'U> =
        async {
            let! result = op
            return f (result)
        }

module Map =
    let merge (m1: Map<'K, 'V>) (m2: Map<'K, 'V>): Map<'K, 'V> =
        Map.fold (fun acc k v -> Map.add k v acc) m1 m2

    let keys (map: Map<'K, 'V>): seq<'K> =
        Map.fold (fun acc k v -> Seq.append acc [ k ]) Seq.empty map

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't

type Deferred<'t> =
    | NotStarted
    | InProgress
    | Resolved of 't

type HackerNewsItem =
    { id: int
      title: string
      url: string option
      score: int
      time: int option }

type Stories =
    | New
    | Top
    | Best
    | Job

type Msg =
    | LoadStoryItem of (int * Result<HackerNewsItem, string>)
    | LoadStoryItems of AsyncOperationStatus<Result<int list, string>>
    | ChangeDisplayedStoriesType of Stories
    | GetMoreStories

type DeferredResult<'t> = Deferred<Result<'t, string>>

type DeferredStoryItem = DeferredResult<HackerNewsItem>

type State =
    { CurrentStories: Stories
      HackerNewsTopStories: DeferredResult<Map<int, DeferredStoryItem>>
      DisplayedStoriesCount: int }

let init (): State * Cmd<Msg> =
    { CurrentStories = New
      HackerNewsTopStories = NotStarted
      DisplayedStoriesCount = 10 },
    Cmd.ofMsg (LoadStoryItems Started)

let topStoriesEndpoint =
    "https://hacker-news.firebaseio.com/v0/topstories.json"

let newStoriesEndpoint =
    "https://hacker-news.firebaseio.com/v0/newstories.json"

let bestStoriesEndpoint =
    "https://hacker-news.firebaseio.com/v0/beststories.json"

let jobStoriesEndpoint =
    "https://hacker-news.firebaseio.com/v0/jobstories.json"

let displayCountIncreaseStepSize: int = 10

let loadTopStoryItemInfo (id: int): Async<Msg> =
    async {
        printf "Requesting item #%i" id
        let endpoint =
            sprintf "https://hacker-news.firebaseio.com/v0/item/%i.json" id

        let! (statusCode, textResponse) = Http.get endpoint
        match statusCode with
        | 200 ->
            let decodingResult =
                Decode.Auto.fromString<HackerNewsItem> textResponse

            return LoadStoryItem(id, decodingResult)
        | otherwise -> return LoadStoryItem(id, Error textResponse)
    }

let storiesEndpoint (storiesType: Stories) =
    match storiesType with
    | New -> newStoriesEndpoint
    | Top -> topStoriesEndpoint
    | Best -> bestStoriesEndpoint
    | Job -> jobStoriesEndpoint

let loadStoryItemsIds (storiesType: Stories): Async<Msg> =
    async {
        let endpointToCall = storiesEndpoint storiesType
        let! (statusCode, topStoriesTextResponse) = Http.get endpointToCall
        match statusCode with
        | 200 ->
            let topStoriesDecodingResult =
                Decode.fromString (Decode.list Decode.int) topStoriesTextResponse

            match topStoriesDecodingResult with
            | Ok topStoriesIds -> return LoadStoryItems(Finished(Ok topStoriesIds))
            | Error decodingError -> return LoadStoryItems(Finished(Error decodingError))
        | otherwise -> return LoadStoryItems(Finished(Error topStoriesTextResponse))
    }

let resetDisplayedStoriesCount (state: State) =
    { state with DisplayedStoriesCount = 10 }

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | ChangeDisplayedStoriesType newStoriesType ->
        { resetDisplayedStoriesCount state with
              CurrentStories = newStoriesType
              HackerNewsTopStories = InProgress },
        Cmd.fromAsync (loadStoryItemsIds newStoriesType)
    | LoadStoryItems Started ->
        { state with
              HackerNewsTopStories = InProgress },
        Cmd.fromAsync (loadStoryItemsIds state.CurrentStories)
    | LoadStoryItems (Finished (Ok storyItems)) ->
        let storiesToDownload =
            storyItems
            |> List.truncate state.DisplayedStoriesCount
            |> List.map (fun x -> (x, InProgress))
            |> Map.ofList

        let storiesNotToDownloadYet =
            storyItems
            |> List.skip state.DisplayedStoriesCount
            |> List.map (fun x -> (x, NotStarted))
            |> Map.ofList

        let newStoriesState =
            Map.merge storiesToDownload storiesNotToDownloadYet

        { state with
              HackerNewsTopStories = Resolved(Ok newStoriesState) },
        Cmd.batch [ for id in Map.keys storiesToDownload -> Cmd.fromAsync (loadTopStoryItemInfo id) ]
    | LoadStoryItems (Finished (Error errorMessage)) ->
        { state with
              HackerNewsTopStories = Resolved(Error errorMessage) },
        Cmd.none
    | LoadStoryItem (itemId, (Ok item)) ->
        match state.HackerNewsTopStories with
        | Resolved (Ok currentTopStories) ->
            let newStoriesState =
                currentTopStories
                |> Map.map (fun k v -> if k = itemId then Resolved(Ok item) else v)

            { state with
                  HackerNewsTopStories = (Resolved(Ok newStoriesState)) },
            Cmd.none
        | otherwise -> state, Cmd.none
    | LoadStoryItem (itemId, Error reason) ->
        match state.HackerNewsTopStories with
        | Resolved (Ok currentTopStories) ->
            let newStoriesState =
                (currentTopStories
                 |> Map.map (fun k v -> if k = itemId then (Resolved(Error reason)) else v))

            { state with
                  HackerNewsTopStories = (Resolved(Ok newStoriesState)) },
            Cmd.none
        | otherwise -> state, Cmd.none
    | GetMoreStories ->
        match state.HackerNewsTopStories with
        | Resolved (Ok currentTopStories) ->
            let storiesIdToDownload =
                currentTopStories
                |> Map.filter (fun k v -> v = NotStarted)
                |> Map.toList
                |> List.map (fun (k, v) -> k)
                |> List.sortDescending
                |> List.truncate displayCountIncreaseStepSize

            let newStoriesState: Map<int, DeferredStoryItem> =
                currentTopStories 
                |> Map.map (fun x y -> if (List.contains x storiesIdToDownload) then InProgress else y)

            let newState =
                { state with
                      DisplayedStoriesCount = state.DisplayedStoriesCount + displayCountIncreaseStepSize
                      HackerNewsTopStories = Resolved (Ok newStoriesState) }

            newState, Cmd.batch [ for id in storiesIdToDownload -> Cmd.fromAsync (loadTopStoryItemInfo id) ]
        | otherwise -> state, Cmd.none

let renderLoading =
    Html.span
        [ Html.progress
            [ prop.classes [ "progress" ]
              prop.max 100 ] ]

let renderError (errorMessage: string) =
    Html.span
        [ prop.style [ style.color.red ]
          prop.text errorMessage ]

let renderTimeItemAdded (storyItem: DeferredStoryItem) =
    match storyItem with
    | Resolved (Ok item) ->
        match item.time with
        | Some t ->
            let datePosted =
                DateTimeOffset.FromUnixTimeSeconds(int64 t).LocalDateTime

            let formatted =
                datePosted.FormatDistanceToNow(createEmpty<IFormatDistanceOptions>)

            Html.span [ prop.text (formatted + " ago") ]
        | otherwise -> Html.none
    | otherwise -> Html.none

let renderItem (itemId: int, storyItem: DeferredStoryItem) =
    let renderedItem =
        match storyItem with
        | NotStarted -> Html.none
        | InProgress -> renderLoading
        | Resolved (Ok item) ->
            Html.span
                [ prop.children
                    [ match item.url with
                      | Some url ->
                          Html.a
                              [ prop.style [ style.textDecoration.underline ]
                                prop.text item.title
                                prop.href url
                                prop.target.blank ]
                      | None -> Html.span item.title
                      Html.span
                          [ prop.classes [ "icon"; "is-medium" ]
                            prop.style [ style.marginLeft 15 ]
                            prop.children
                                [ Html.i
                                    [ prop.classes [ "fa"; "fa-dot-circle" ]
                                      prop.text item.score ] ] ] ] ]
        | Resolved (Error error) -> renderError error

    Html.div
        [ prop.id itemId
          prop.classes [ "box" ]
          prop.style
              [ style.marginTop 15
                style.marginBottom 15 ]
          prop.children
              [ Html.nav
                  [ prop.classes [ "level" ]
                    prop.children
                        [ Html.div
                            [ prop.classes [ "level-left" ]
                              prop.children
                                  [ Html.div
                                      [ prop.classes [ "level-item" ]
                                        prop.children
                                            [ Html.span
                                                [ prop.classes [ "icon"; "is-large" ]
                                                  prop.children
                                                      [ Html.i
                                                          [ prop.classes
                                                              [ "fa"
                                                                "fa-angle-double-right"
                                                                "fa-x" ] ] ] ]
                                              renderedItem ] ] ] ]
                          Html.div
                              [ prop.classes [ "level-right" ]
                                prop.children
                                    [ Html.div
                                        [ prop.classes [ "level-item" ]
                                          prop.children [ renderTimeItemAdded storyItem ] ] ] ] ] ] ] ]

let renderItems (storyItems: Map<int, DeferredStoryItem>) (count: int): Fable.React.ReactElement =
    storyItems
    |> Map.toList
    |> List.sortByDescending (fun (id, item) ->
        match item with
        | Resolved (Ok resolvedItem) ->
            match resolvedItem.time with
            | Some t -> item, t
            | otherwise -> item, 0
        | otherwise -> item, 0)
    |> List.truncate count
    |> List.map renderItem
    |> Html.div

let storyCategories =
    [ Stories.New
      Stories.Top
      Stories.Best
      Stories.Job ]

let storiesName (stories: Stories): string =
    match stories with
    | New -> "New"
    | Top -> "Top"
    | Best -> "Best"
    | Job -> "Job"

let storiesFontAwesomeIcon (stories: Stories): string =
    match stories with
    | New -> "fa-newspaper"
    | Top -> "fa-chevron-up"
    | Best -> "fa-fire"
    | Job -> "fa-users"

let renderTitle =
    Html.div
        [ prop.classes [ "container"; "mb-6" ]
          prop.children
              [ Html.h1
                  [ prop.classes [ "title"; "is-large" ]
                    prop.text "Hackernews app" ] ] ]

let renderTabs (currentStories: Stories) (dispatch: Msg -> unit) =
    let changeDisplayedStoriesType (newStories: Stories) =
        if currentStories <> newStories
        then dispatch (ChangeDisplayedStoriesType newStories)

    Html.div
        [ prop.classes
            [ "tabs"
              "is-centered"
              "is-toggle"
              "is-toggle-rounded"
              "is-large" ]
          prop.children
              [ Html.ul
                  [ prop.children
                      [ for storyType in storyCategories ->
                          Html.li
                              [ prop.classes [ if storyType = currentStories then "is-active" ]
                                prop.children
                                    [ Html.a
                                        [ prop.onClick (fun _ -> changeDisplayedStoriesType storyType)
                                          prop.children
                                              [ Html.span
                                                  [ prop.classes [ "icon"; "is-small" ]
                                                    prop.children
                                                        [ Html.i
                                                            [ prop.classes
                                                                [ "fas"
                                                                  storiesFontAwesomeIcon storyType ]
                                                              prop.ariaHidden true ] ] ]
                                                Html.span [ prop.text (storiesName storyType) ] ] ] ] ] ] ] ] ]

let renderLoadMoreButton (dispatch: Msg -> unit) =
    Html.div
        [ prop.classes [ "columns" ]
          prop.children
              [ Html.div
                  [ prop.classes
                      [ "column"
                        "is-offset-one-quarter"
                        " is-half" ]
                    prop.children
                        [ Html.a
                            [ prop.classes
                                [ "button"
                                  "is-primary"
                                  "is-fullwidth"
                                  "is-outlined" ]
                              prop.text "Load more"
                              prop.onClick (fun _ -> dispatch GetMoreStories) ] ] ] ] ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.section
        [ prop.classes [ "section" ]
          prop.children
              [ renderTitle
                renderTabs state.CurrentStories dispatch
                match state.HackerNewsTopStories with
                | NotStarted -> renderLoading
                | InProgress -> renderLoading
                | Resolved (Ok items) ->
                    Html.div
                        [ prop.classes [ "container" ]
                          prop.children
                              [ renderItems items state.DisplayedStoriesCount
                                if (Map.count items) > state.DisplayedStoriesCount
                                then renderLoadMoreButton dispatch else Html.none ] ]
                | Resolved (Error error) -> renderError error ] ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
