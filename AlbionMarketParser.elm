module AlbionMarketParser exposing (..)

import Json.Decode exposing (string, int, float, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Resources =
    { resources : List MarketDataItem }


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
