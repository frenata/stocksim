port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, class, type_, name, checked, id, style, autofocus, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (float, string, int, Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, requiredAt)
import Dict exposing (Dict)
import Round


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--MODEL


type alias Model =
    { quote : Maybe Quote
    , symbol : String
    , orders : Int
    , error : Maybe String
    , portfolio : Portfolio
    }


type alias Portfolio =
    { balance : Float, positions : Dict String Position }


type alias Position =
    { name : String, shares : Int, price : Float }


type alias Quote =
    { symbol : String, name : String, ask : Float, bid : Float }


init : Maybe ( Float, List ( String, Position ) ) -> ( Model, Cmd Msg )
init storage =
    let
        portfolio =
            case storage of
                Just ( balance, storedPortfolio ) ->
                    (Portfolio balance (Dict.fromList storedPortfolio))

                Nothing ->
                    blankPortfolio
    in
        ( { quote = Nothing
          , symbol = ""
          , orders = 1
          , error = Just "Search for stock symbols and build your portfolio."
          , portfolio = portfolio
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
    | NewOrders String
    | GetSymbol String
    | BuyStock
    | SellStock
    | IncOrders
    | DecOrders
    | ResetPortfolio


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestQuote (Ok quote) ->
            ( { model | quote = Just quote, error = Nothing }, Cmd.none )

        RequestQuote (Err e) ->
            ( { model | error = Just "Symbol unrecognized." }, Cmd.none )

        NewSymbol symbol ->
            ( { model | symbol = String.toUpper symbol }, Cmd.none )

        NewOrders input ->
            let
                orders =
                    Result.withDefault model.orders (String.toInt input)
            in
                if orders <= 1 then
                    ( { model | orders = 1 }, Cmd.none )
                else
                    ( { model | orders = orders }, Cmd.none )

        GetSymbol symbol ->
            ( { model | symbol = symbol }, pullQuote symbol )

        BuyStock ->
            storeModel <| buy model

        SellStock ->
            storeModel <| sell model

        IncOrders ->
            ( { model | orders = model.orders + 1 }, Cmd.none )

        DecOrders ->
            if model.orders <= 1 then
                ( { model | orders = 1 }, Cmd.none )
            else
                ( { model | orders = model.orders - 1 }, Cmd.none )

        ResetPortfolio ->
            ( { model | portfolio = blankPortfolio }, removeStorage (storePortfolio model.portfolio) )



--QUOTE functions


buy : Model -> Model
buy model =
    case model.quote of
        Just quote ->
            let
                shares =
                    newShares model.portfolio.positions quote model.orders

                price =
                    newPrice model.portfolio.positions quote model.orders

                balance =
                    model.portfolio.balance - (quote.ask * (toFloat model.orders))

                positions =
                    updatePosition model.portfolio.positions quote price shares
            in
                if balance < 0 then
                    { model | orders = Basics.floor (model.portfolio.balance / quote.ask) }
                else
                    { model | portfolio = (Portfolio balance positions) }

        Nothing ->
            model


sell : Model -> Model
sell model =
    case model.quote of
        Just quote ->
            let
                shares =
                    newShares model.portfolio.positions quote -model.orders

                blankShares =
                    newShares model.portfolio.positions quote 0

                price =
                    newPrice model.portfolio.positions quote 0

                balance =
                    model.portfolio.balance + (quote.bid * (toFloat model.orders))

                positions =
                    updatePosition model.portfolio.positions quote price shares
            in
                if shares < 0 && blankShares > 0 then
                    { model | orders = blankShares }
                else if shares < 0 then
                    { model | orders = 1 }
                else if shares == 0 then
                    { model | portfolio = (Portfolio balance (Dict.remove quote.symbol positions)) }
                else
                    { model | portfolio = (Portfolio balance positions) }

        Nothing ->
            model


newShares : Dict String Position -> Quote -> Int -> Int
newShares positions quote n =
    case Dict.get quote.symbol positions of
        Just old ->
            old.shares + n

        Nothing ->
            n


newPrice : Dict String Position -> Quote -> Int -> Float
newPrice positions quote n =
    case Dict.get quote.symbol positions of
        Just old ->
            (old.price * (toFloat old.shares) + quote.ask * (toFloat n)) / toFloat (old.shares + n)

        Nothing ->
            quote.ask


updatePosition : Dict String Position -> Quote -> Float -> Int -> Dict String Position
updatePosition positions quote price shares =
    Dict.insert quote.symbol (Position quote.name shares price) positions


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
        [ h3 [] [ text "Simple Stock Simulator" ]
        , viewError model.error
        , div [ id "main" ]
            [ viewQuote model
            , viewOrders model.orders
            , viewPortfolio model.portfolio
            ]
        ]


viewSymbol : String -> Html Msg
viewSymbol symbol =
    form [ id "symbol", onSubmit (GetSymbol symbol) ]
        [ input
            [ onInput NewSymbol
            , placeholder "Stock symbol"
            , autofocus True
            , value symbol
            ]
            []
        ]


viewQuote : Model -> Html Msg
viewQuote model =
    case model.quote of
        Just quote ->
            div [ id "quote" ]
                [ viewSymbol model.symbol
                , p [] [ text quote.name ]
                , p [] [ text ("Buy: $" ++ (toString quote.ask)) ]
                , p [] [ text ("Sell: $" ++ (toString quote.bid)) ]
                ]

        Nothing ->
            div [ id "quote" ] [ viewSymbol model.symbol ]


viewOrders : Int -> Html Msg
viewOrders orders =
    div [ id "order" ]
        [ button [ onClick BuyStock ] [ text "Buy" ]
        , button [ onClick IncOrders ] [ text "+" ]
        , input [ onInput NewOrders, placeholder "# of orders", value (toString orders) ] []
        , button [ onClick DecOrders ] [ text "-" ]
        , button [ onClick SellStock ] [ text "Sell" ]
        ]


viewError : Maybe String -> Html Msg
viewError error =
    case error of
        Nothing ->
            p [ id "error" ] []

        Just error ->
            p [ id "error" ] [ text error ]


viewPortfolio : Portfolio -> Html Msg
viewPortfolio portfolio =
    let
        list =
            Dict.toList portfolio.positions
    in
        div [ id "portfolio" ]
            [ div [ id "header" ]
                [ hr [] []
                , p [ id "title" ]
                    [ text "Portfolio  "
                    , button [ onClick ResetPortfolio ] [ text "Reset" ]
                    ]
                , p [] [ text <| "Cash: $" ++ Round.round 2 portfolio.balance ]
                ]
            , table [ id "positions" ]
                (tr []
                    [ th [] [ text "Symbol" ]
                    , th [] [ text "Name" ]
                    , th [] [ text "Shares" ]
                    , th [] [ text "Avg Price" ]
                    ]
                    :: (List.map (\p -> printPosition p) list)
                )
            ]


printPosition : ( String, Position ) -> Html Msg
printPosition position =
    let
        ( symbol, details ) =
            position
    in
        tr []
            [ td [] [ text symbol ]
            , td [] [ text details.name ]
            , td [] [ text <| toString details.shares ]
            , td [] [ text <| Round.round 2 details.price ]
            , td [] [ button [ onClick (GetSymbol symbol) ] [ text "Get Quote" ] ]
            ]



--PORTS


storePortfolio : Portfolio -> ( Float, List ( String, Position ) )
storePortfolio portfolio =
    ( portfolio.balance, Dict.toList portfolio.positions )


storeModel : Model -> ( Model, Cmd Msg )
storeModel model =
    ( model, setStorage (storePortfolio model.portfolio) )


port setStorage : ( Float, List ( String, Position ) ) -> Cmd msg


port removeStorage : ( Float, List ( String, Position ) ) -> Cmd msg



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
