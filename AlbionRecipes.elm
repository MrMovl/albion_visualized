module AlbionRecipes exposing (..)

import Http
import Html exposing (..)
import Html.Attributes exposing (class)
import AlbionMarketParser exposing (..)


initialCmd : Cmd Msg
initialCmd =
    resourcesDecoder
        |> Http.get "https://albion-market.com/api/v1/orders/resources/"
        |> Http.send LoadItems


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


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none)
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
