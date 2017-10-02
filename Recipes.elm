module Recipes exposing (..)


type IngredientType
    = NoIngredient
    | Carrots
    | Beans
    | Bread
    | Sheeps_Butter


type Product
    = BeanSalad


type alias Ingredient =
    { ingredientType : IngredientType
    , amountNeeded : Int
    }


type alias Recipe =
    { ingredient1 : Maybe Ingredient
    , ingredient2 : Maybe Ingredient
    , ingredient3 : Maybe Ingredient
    , ingredient4 : Maybe Ingredient
    , product : Product
    }


allProducts : List Product
allProducts =
    [ BeanSalad
    ]


noIngredient : Ingredient
noIngredient =
    { ingredientType = NoIngredient
    , amountNeeded = 0
    }


beanSaladRecipe : Recipe
beanSaladRecipe =
    { ingredient1 = Just { ingredientType = Beans, amountNeeded = 16 }
    , ingredient2 = Just { ingredientType = Carrots, amountNeeded = 16 }
    , ingredient3 = Nothing
    , ingredient4 = Nothing
    , product = BeanSalad
    }
