module Knapsack exposing (solve_01, Value, Weight, WeightLimit)

import List


type alias Value =
    Int


type alias Weight =
    Int


type alias WeightLimit =
    Int


solve_01 : List a -> (a -> Value) -> (a -> Weight) -> WeightLimit -> Maybe (List a)
solve_01 rawItems toValue toWeight limit =
    let
        mappedItems =
            List.map (\item -> ( item, toWeight item, toValue item )) rawItems

        addItem ( item, weight, value ) items =
            let
                ( left, right ) =
                    ( List.take weight items, List.drop weight items )

                optimalList =
                    List.map (\( v, i ) -> ( v + value, item :: i )) items

                chooseOptimal ( left_value, left_items ) ( right_value, right_items ) =
                    if left_value > right_value then
                        ( left_value, left_items )
                    else
                        ( right_value, right_items )
            in
                left ++ List.map2 chooseOptimal right optimalList

        optimalWeights =
            List.foldr addItem (List.repeat (limit + 1) ( 0, [] )) mappedItems
    in
        optimalWeights
            |> List.foldl (Just >> always) Nothing
            |> Maybe.map Tuple.second
