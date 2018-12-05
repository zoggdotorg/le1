module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes
import Random
import Random.List exposing (shuffle)


main : Program () Model Msg
main =
    Browser.element
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
    , outcomeText : String
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
        [ Advancement "Juno Rockets" 3 "The firing of the Juno" []
        , Advancement "Soyuz Rockets" 3 "The firing of the Soyuz" []
        , Advancement "Atlas Rockets" 3 "The firing of the Atlas" []
        , Advancement "Saturn Rockets" 3 "The firing of the Saturn" []
        , Advancement "Ion Thrusters" 3 "The firing of the Ion Thruster" []
        , Advancement "Landing" 3 "The landing" []
        , Advancement "Life Support" 3 "The use of life support" []
        , Advancement "Re-entry" 3 "The re-entry" []
        , Advancement "Rendezvous" 3 "The rendezvous" []
        , Advancement "Surveying" 1 "The survey" []
        ]


initialOutcomes : Deck
initialOutcomes =
    List.concat
        [ List.repeat 60 Success
        , List.repeat 15 MinorFailure
        , List.repeat 15 MajorFailure
        ]


init : () -> ( Model, Cmd Msg )
init _ =
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
    | UpdateAdvOutcomes Advancement Deck
    | UpdateDiscards Advancement Deck
    | ShuffleOutcomes Deck
    | ShowUnresearched
    | ShowResearched


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

        ShowResearched ->
            ( { model | page = Advancements }, Cmd.none )


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


updateAdvOutcomes : Model -> Advancement -> Deck -> Model
updateAdvOutcomes model adv outcomes =
    -- Replace adv  with supplied list.
    { model | advancements = updateAdv model.advancements { adv | outcomes = outcomes } }


updateDiscards : Model -> Advancement -> Deck -> Model
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
            outcomePage model adv

        ChooseAdvancement ->
            chooseAdvancementPage model


outcomePage : Model -> Advancement -> Html Msg
outcomePage model adv =
    let
        testOption : String -> Int -> String -> Html Msg
        testOption desc cost colour =
            div [ Html.Attributes.style "backgroundColor" colour ]
                [ text (adv.outcomeText ++ " was a " ++ desc ++ ".")
                , br [] []
                , button [ onClick (DiscardOutcome adv) ] [ text ("Discard for $" ++ String.fromInt cost) ]
                , if cost > 0 then
                    text " or "
                  else
                    text ""
                , if cost > 0 then
                    button [ onClick (ReplaceOutcome adv) ] [ text "Replace outcome" ]
                  else
                    text ""
                ]
    in
        case List.head adv.outcomes of
            Just Success ->
                if List.length adv.outcomes == 1 then
                    testOption "Success, and the last outcome" 0 " green"
                else
                    testOption "Success" 10 "green"

            Just MinorFailure ->
                testOption "Minor Failure" 5 "orange"

            Just MajorFailure ->
                testOption "Major Failure" 5 "red"

            Nothing ->
                div [] [ text "Something blew up on the pad!" ]


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
            , div [] (List.filterMap unresearchedAdv model.advancements)
            , p [] []
            , div [] [ button [ onClick ShowResearched ] [ text "cancel" ] ]
            ]


advancementsPage : Model -> Html Msg
advancementsPage model =
    let
        currentAdv : Advancement -> Maybe (Html Msg)
        currentAdv adv =
            if adv.initial == 0 then
                div []
                    [ if List.length (adv.outcomes) > 0 then
                        button [ onClick (TestOutcome adv) ]
                            [ text ("Use " ++ adv.name ++ " (" ++ String.fromInt (List.length (adv.outcomes)) ++ ")") ]
                      else
                        button [ Html.Attributes.disabled True ]
                            [ text (adv.name ++ " complete") ]
                    ]
                    |> Just
            else
                Nothing
    in
        div []
            [ h3
                []
                [ text "Advancements" ]
            , div []
                (List.filterMap currentAdv model.advancements)
            , p [] []
            , button [ onClick ShowUnresearched ] [ text "Research Advancement" ]
            ]
