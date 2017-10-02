module AlbionRecipes exposing (..)

import Http
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, on)
import Recipes exposing (..)


type alias Model =
    { selectedRecipe : Maybe Product
    , loadingError : Maybe Http.Error
    }


initialModel : Model
initialModel =
    { selectedRecipe = Nothing
    , loadingError = Nothing
    }


type Msg
    = NoRecipe
    | SelectRecipe Product


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectRecipe product ->
            ( { model | selectedRecipe = Just product }, Cmd.none )

        NoRecipe ->
            ( { model | selectedRecipe = Nothing }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


view : Model -> Html Msg
view model =
    let
        selectedRecipe =
            case model.selectedRecipe of
                Nothing ->
                    text ""

                Just product ->
                    product |> toRecipe |> printRecipe
    in
        div [ class "content" ]
            [ h1 [] [ text "Albion Recipes" ]
            , div []
                [ printAllProducts
                , selectedRecipe
                ]
            ]


toRecipe : Product -> Recipe
toRecipe product =
    case product of
        BeanSalad ->
            beanSaladRecipe


printRecipe : Recipe -> Html Msg
printRecipe recipe =
    div [ class "column" ]
        [ div []
            [ printProduct recipe.product
            , printIngredient recipe.ingredient1
            , printIngredient recipe.ingredient2
            , printIngredient recipe.ingredient3
            , printIngredient recipe.ingredient4
            ]
        ]


printProduct : Product -> Html Msg
printProduct product =
    h4 [ onClick (SelectRecipe product) ] [ product |> productTypeToString |> text ]


productTypeToString : Product -> String
productTypeToString product =
    case product of
        BeanSalad ->
            "Bean Salad"


printIngredient : Maybe Ingredient -> Html Msg
printIngredient ingredient =
    let
        processedIngredient =
            ingredient |> Maybe.withDefault noIngredient
    in
        div []
            [ processedIngredient
                |> .ingredientType
                |> ingredientTypeToString
                |> text
            , processedIngredient
                |> .amountNeeded
                |> toString
                |> (++) " -- "
                |> text
            ]


ingredientTypeToString : IngredientType -> String
ingredientTypeToString ingredientType =
    case ingredientType of
        NoIngredient ->
            "none"

        Carrots ->
            "Carrots"

        Beans ->
            "Beans"

        Bread ->
            "Bread"

        Sheeps_Butter ->
            "Sheep's Butter"


printAllProducts : Html Msg
printAllProducts =
    List.map printProduct allProducts |> div [ class "column" ]
