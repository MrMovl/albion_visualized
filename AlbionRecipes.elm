module AlbionRecipes exposing (..)

--import Html.Events exposing (onClick)
--import Array exposing (Array)
--import Random

import Http
import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title)
import Json.Decode exposing (string, int, float, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


view : Model -> Html Msg
view model =
    let
        itemList =
            case model.items of
                Nothing ->
                    text "Loading..."

                Just items ->
                    printItemList items
    in
        div [ class "content" ]
            [ h1 [] [ text "Albion Market" ]
            , itemList
            ]


printItemList : List MarketDataItem -> Html Msg
printItemList items =
    List.map
        printMarketDataItem
        items
        |> div []


printMarketDataItem : MarketDataItem -> Html Msg
printMarketDataItem item =
    div []
        [ printItem item.item
        , printStats item.stats
        ]


printStats : Stats -> Html Msg
printStats stats =
    div []
        [ div [] [ h4 [] [ text "Buy" ], printBuySellStats stats.buy ]
        , div [] [ h4 [] [ text "Sell" ], printBuySellStats stats.sell ]
        ]


printBuySellStats : BuySell -> Html Msg
printBuySellStats stats =
    ul []
        [ li [] [ toString stats.total_volume |> (++) "Total Volume: " |> text ]
        , li [] [ toString stats.price_average |> (++) "Price Average: " |> text ]
        , li [] [ toString stats.price_minimum |> (++) "Price Minimum: " |> text ]
        , li [] [ toString stats.price_maximum |> (++) "Price Maximum: " |> text ]
        , li [] [ toString stats.order_count |> (++) "Order Count: " |> text ]
        ]


printItem : Item -> Html Msg
printItem item =
    div [] [ h2 [] [ text item.name ] ]


type alias Model =
    { items : Maybe (List MarketDataItem)
    , loadingError : Maybe Http.Error
    }


initialModel : Model
initialModel =
    { items = Nothing
    , loadingError = Nothing
    }


type Msg
    = LoadItems (Result Http.Error Resources)
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadItems (Ok items) ->
            ( { model | items = Just items.resources }, Cmd.none )

        LoadItems (Err error) ->
            ( { model | loadingError = Just error }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


initialCmd : Cmd Msg
initialCmd =
    resourcesDecoder
        |> Http.get "https://albion-market.com/api/v1/orders/resources/"
        |> Http.send LoadItems


type alias Resources =
    { resources : List MarketDataItem }


resourcesDecoder : Decoder Resources
resourcesDecoder =
    decode buildResources
        |> required "resources" (list marketItemDecoder)


buildResources : List MarketDataItem -> Resources
buildResources marketDataItems =
    { resources = marketDataItems }


marketItemDecoder : Decoder MarketDataItem
marketItemDecoder =
    decode buildMarketItem
        |> required "stats" statsDecoder
        |> required "item" itemDecoder


statsDecoder : Decoder Stats
statsDecoder =
    decode buildStats
        |> required "buy" buySellDecoder
        |> required "sell" buySellDecoder


buySellDecoder : Decoder BuySell
buySellDecoder =
    decode buildBuySell
        |> required "total_volume" int
        |> required "price_average" float
        |> required "price_minimum" int
        |> required "price_maximum" int
        |> required "order_count" int


itemDecoder : Decoder Item
itemDecoder =
    decode Item
        |> required "id" string
        |> required "name" string
        |> required "category_id" string
        |> required "category_name" string
        |> required "sub_category_id" string
        |> required "sub_category_name" string
        |> required "tier" int


buildMarketItem : Stats -> Item -> MarketDataItem
buildMarketItem stats item =
    { stats = stats
    , item = item
    }


buildStats : BuySell -> BuySell -> Stats
buildStats buy sell =
    { buy = buy
    , sell = sell
    }


buildBuySell : Int -> Float -> Int -> Int -> Int -> BuySell
buildBuySell total_volume price_average price_minimum price_maximum order_count =
    { total_volume = total_volume
    , price_average = price_average
    , price_minimum = price_minimum
    , price_maximum = price_maximum
    , order_count = order_count
    }


buildItem : String -> String -> String -> String -> String -> String -> Int -> Item
buildItem id name category_id category_name sub_category_id sub_category_name tier =
    { id = id
    , name = name
    , category_id = category_id
    , category_name = category_name
    , sub_category_id = sub_category_id
    , sub_category_name = sub_category_name
    , tier = tier
    }


type alias MarketDataItem =
    { stats : Stats
    , item : Item
    }


type alias Stats =
    { buy : BuySell
    , sell : BuySell
    }


type alias BuySell =
    { total_volume : Int
    , price_average : Float
    , price_minimum : Int
    , price_maximum : Int
    , order_count : Int
    }


type alias Item =
    { id : String
    , name : String
    , category_id : String
    , category_name : String
    , sub_category_id : String
    , sub_category_name : String
    , tier : Int
    }


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just error ->
            div [ class "error-message" ]
                [ h1 [] [ text "Albion Recipes" ]
                , p [] [ printError error ]
                ]


printError : Http.Error -> Html Msg
printError error =
    case error of
        Http.BadUrl string ->
            text ("Bad URL:" ++ string)

        Http.Timeout ->
            text "Timeout"

        Http.NetworkError ->
            text "Network Error"

        Http.BadStatus _ ->
            text "Bad Status"

        Http.BadPayload message _ ->
            "Bad Payload: " ++ message |> text


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
