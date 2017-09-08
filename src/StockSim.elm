module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, class, type_, name, checked, id, style, autofocus)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (float, string, int, Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, requiredAt)
import Task
import Regex


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--MODEL


type alias Model =
    { quote : Maybe Quote
    , symbol : String
    , error : Maybe String
    , portfolio : Portfolio
    }


type alias Portfolio =
    { balance : Float, positions : List Position }


type alias Position =
    { symbol : String, shares : Int, price : Float }


type alias Quote =
    { symbol : String, name : String, ask : Float, bid : Float }


init : ( Model, Cmd Msg )
init =
    ( { quote = Nothing
      , symbol = ""
      , error = Nothing
      , portfolio = blankPortfolio
      }
    , Cmd.none
    )


blankPortfolio : Portfolio
blankPortfolio =
    Portfolio 100000.0 []



--UPDATE


type Msg
    = RequestQuote (Result Http.Error Quote)
    | NewSymbol String
    | GetSymbol


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestQuote (Ok quote) ->
            ( { model | quote = Just quote, error = Nothing }, Cmd.none )

        RequestQuote (Err e) ->
            ( { model | error = Just "Symbol unrecognized." }, Cmd.none )

        NewSymbol symbol ->
            ( { model | symbol = symbol }, Cmd.none )

        GetSymbol ->
            ( model, pullQuote model.symbol )



--QUOTE functions


pullQuote : String -> Cmd Msg
pullQuote symbol =
    let
        url =
            "https://cors-anywhere.herokuapp.com/http://data.benzinga.com/rest/richquoteDelayed?symbols="
                ++ symbol
    in
        Http.send RequestQuote <| get url (decodeQuote symbol)


get : String -> Decoder Quote -> Http.Request Quote
get url decoder =
    Http.request
        { method = "GET"
        , headers =
            []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (decoder)
        , timeout = Nothing
        , withCredentials = False
        }


decodeQuote : String -> Decoder Quote
decodeQuote symbol =
    decode Quote
        |> requiredAt [ symbol, "symbol" ] string
        |> requiredAt [ symbol, "name" ] string
        |> requiredAt [ symbol, "askPrice" ] float
        |> requiredAt [ symbol, "bidPrice" ] float



--VIEW


view : Model -> Html Msg
view model =
    div
        [ id "app" ]
        [ viewQuote model.quote
        , viewError model.error
        , form [ onSubmit GetSymbol ]
            [ input [ onInput NewSymbol, placeholder "Stock symbol", autofocus True ] [] ]
        ]


viewQuote : Maybe Quote -> Html Msg
viewQuote quote =
    case quote of
        Just quote ->
            div [ id "quote" ]
                [ p [] [ text quote.symbol ]
                , p [] [ text quote.name ]
                , p [] [ text (toString quote.ask) ]
                , p [] [ text (toString quote.bid) ]
                ]

        Nothing ->
            div [ id "quote" ] []


viewError : Maybe String -> Html Msg
viewError error =
    case error of
        Nothing ->
            p [] []

        Just error ->
            p [] [ text error ]



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
