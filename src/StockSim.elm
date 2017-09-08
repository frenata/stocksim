module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, class, type_, name, checked, id, style, autofocus)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (float, string, int, Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, requiredAt)
import Task
import Regex
import Dict exposing (Dict)


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
    { balance : Float, positions : Dict String Position }


type alias Position =
    { name : String, shares : Int, price : Float }


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
    Portfolio 100000.0 Dict.empty



--UPDATE


type Msg
    = RequestQuote (Result Http.Error Quote)
    | NewSymbol String
    | GetSymbol
    | BuyStock
    | SellStock


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

        BuyStock ->
            ( { model | portfolio = buy model.portfolio model.quote 1 }, Cmd.none )

        SellStock ->
            ( { model | portfolio = sell model.portfolio model.quote -1 }, Cmd.none )



--QUOTE functions


buy : Portfolio -> Maybe Quote -> Int -> Portfolio
buy portfolio quote n =
    case quote of
        Just quote ->
            { portfolio
                | balance = portfolio.balance - quote.ask
                , positions = updatePosition portfolio.positions quote n
            }

        Nothing ->
            portfolio


sell : Portfolio -> Maybe Quote -> Int -> Portfolio
sell portfolio quote n =
    case quote of
        Just quote ->
            { portfolio
                | balance = portfolio.balance + quote.bid
                , positions = updatePosition portfolio.positions quote n
            }

        Nothing ->
            portfolio


updatePosition : Dict String Position -> Quote -> Int -> Dict String Position
updatePosition positions quote num =
    let
        oldPosition =
            Dict.get quote.symbol positions
    in
        case oldPosition of
            Just oldPosition ->
                Dict.insert quote.symbol (Position quote.name (oldPosition.shares + num) quote.ask) positions

            Nothing ->
                Dict.insert quote.symbol (Position quote.name num quote.ask) positions


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
        , button [ onClick BuyStock ] [ text "Buy" ]
        , button [ onClick SellStock ] [ text "Sell" ]
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
