module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Random
import Random.List exposing (shuffle)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { advancements : List Advancement
    , outcomes : Deck
    , discards : Deck
    , page : Page
    }


type Page
    = Advancements
    | ChooseOutcome Advancement
    | ChooseAdvancement


type alias Advancement =
    { name : String
    , initial : Int
    , outcomes : Deck
    }


type alias Deck =
    List Outcome


type Outcome
    = Success
    | MinorFailure
    | MajorFailure


initialAdvancements : List Advancement
initialAdvancements =
    List.sortBy .name
        [ Advancement "Juno Rockets" 3 []
        , Advancement "Soyuz Rockets" 3 []
        , Advancement "Atlas Rockets" 3 []
        , Advancement "Saturn Rockets" 3 []
        , Advancement "Ion Thrusters" 3 []
        , Advancement "Landing" 3 []
        , Advancement "Life Support" 3 []
        , Advancement "Re-entry" 3 []
        , Advancement "Rendezvous" 3 []
        , Advancement "Surveying" 1 []
        ]


initialOutcomes : List Outcome
initialOutcomes =
    List.concat
        [ List.repeat 60 Success
        , List.repeat 15 MinorFailure
        , List.repeat 15 MajorFailure
        ]


init : ( Model, Cmd Msg )
init =
    ( Model
        initialAdvancements
        []
        []
        ChooseAdvancement
    , Random.generate ShuffleOutcomes (shuffle initialOutcomes)
    )



-- UPDATE


type Msg
    = TestOutcome Advancement
    | ResearchAdvancement Advancement
    | DiscardOutcome Advancement
    | ReplaceOutcome Advancement
    | UpdateAdvOutcomes Advancement (List Outcome)
    | UpdateDiscards Advancement (List Outcome)
    | ShuffleOutcomes (List Outcome)
    | ShowUnresearched


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TestOutcome adv ->
            ( { model | page = ChooseOutcome adv }, Cmd.none )

        ResearchAdvancement adv ->
            ( research model adv, Cmd.none )

        ReplaceOutcome adv ->
            ( { model | page = Advancements }, Random.generate (UpdateAdvOutcomes adv) (shuffle adv.outcomes) )

        DiscardOutcome adv ->
            ( { model | page = Advancements }, Random.generate (UpdateDiscards adv) (shuffle ((List.head adv.outcomes |> Maybe.withDefault Success) :: model.discards)) )

        UpdateAdvOutcomes adv outcomes ->
            ( updateAdvOutcomes model adv outcomes, Cmd.none )

        UpdateDiscards adv discards ->
            ( updateDiscards model adv discards, Cmd.none )

        ShuffleOutcomes outcomes ->
            ( { model | outcomes = outcomes }, Cmd.none )

        ShowUnresearched ->
            ( { model | page = ChooseAdvancement }, Cmd.none )


updateAdv : List Advancement -> Advancement -> List Advancement
updateAdv advs adv =
    List.map
        (\elt ->
            if elt.name == adv.name then
                adv
            else
                elt
        )
        advs


updateAdvOutcomes : Model -> Advancement -> List Outcome -> Model
updateAdvOutcomes model adv outcomes =
    -- Replace adv  with supplied list.
    { model | advancements = updateAdv model.advancements { adv | outcomes = outcomes } }


updateDiscards : Model -> Advancement -> List Outcome -> Model
updateDiscards model adv discards =
    -- Replace discards with supplied list.
    -- Remove head from adv
    let
        new =
            if List.length adv.outcomes == 1 then
                { adv | outcomes = [] }
            else
                { adv | outcomes = List.drop 1 adv.outcomes }
    in
        { model
            | discards = discards
            , advancements = updateAdv model.advancements new
        }


research : Model -> Advancement -> Model
research model adv =
    let
        ( res, outcomes, discards ) =
            if adv.initial <= List.length model.outcomes then
                ( (List.take adv.initial model.outcomes), (List.drop adv.initial model.outcomes), model.discards )
            else
                -- Assume there is enough in the discards
                let
                    newdeck =
                        List.concat [ model.outcomes, model.discards ]
                in
                    ( (List.take adv.initial newdeck), (List.drop adv.initial newdeck), [] )
    in
        { model
            | advancements = updateAdv model.advancements { adv | outcomes = res, initial = 0 }
            , outcomes = outcomes
            , discards = discards
            , page = Advancements
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Advancements ->
            advancementsPage model

        ChooseOutcome adv ->
            chooseOutcomePage model adv

        ChooseAdvancement ->
            chooseAdvancementPage model


chooseOutcomePage : Model -> Advancement -> Html Msg
chooseOutcomePage model adv =
    let
        testOption : Advancement -> String -> Int -> Html Msg
        testOption adv desc cost =
            div []
                [ text ("It was a " ++ desc ++ "!")
                , button [ onClick (DiscardOutcome adv) ] [ text ("Discard for $" ++ toString cost) ]
                , if cost > 0 then
                    button [ onClick (ReplaceOutcome adv) ] [ text "Replace outcome" ]
                  else
                    text ""
                ]
    in
        div []
            [ h2 []
                [ text ("Testing " ++ adv.name)
                ]
            , case List.head adv.outcomes of
                Just Success ->
                    if List.length adv.outcomes == 1 then
                        testOption adv "Success, and the last outcome" 0
                    else
                        testOption adv "Success" 10

                Just MinorFailure ->
                    testOption adv "Minor Failure" 5

                Just MajorFailure ->
                    testOption adv "Major Failure" 5

                Nothing ->
                    div [] [ text "Something blew up on the pad!" ]
            ]


chooseAdvancementPage : Model -> Html Msg
chooseAdvancementPage model =
    let
        unresearchedAdv : Advancement -> Maybe (Html Msg)
        unresearchedAdv adv =
            if adv.initial > 0 then
                div [] [ button [ onClick (ResearchAdvancement adv) ] [ text adv.name ] ] |> Just
            else
                Nothing
    in
        div []
            [ h2 [] [ text "Research" ]
            , div []
                (List.filterMap unresearchedAdv model.advancements)
            ]


advancementsPage : Model -> Html Msg
advancementsPage model =
    let
        currentAdv : Advancement -> Maybe (Html Msg)
        currentAdv adv =
            if not (List.isEmpty adv.outcomes) then
                div []
                    [ button [ onClick (TestOutcome adv) ]
                        [ text (adv.name ++ "(" ++ toString (List.length (adv.outcomes)) ++ ")") ]
                    ]
                    |> Just
            else
                Nothing
    in
        div []
            [ h2
                []
                [ text "Test" ]
            , div []
                (List.filterMap currentAdv model.advancements)
            , button [ onClick ShowUnresearched ] [ text "Research Advancement" ]
            ]
