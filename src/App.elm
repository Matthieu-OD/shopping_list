module App exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Ingredient =
    { name : String, quantity : Int, unit : String }


type alias NewIngredient =
    { name : String, quantity : Int, unit : String }


type alias Model =
    { ingredients : List Ingredient, newIngredient : NewIngredient }


init : Model
init =
    { ingredients = [ { name = "apple", quantity = 1, unit = "" } ], newIngredient = { name = "", quantity = 1, unit = "" } }



-- UPDATE
-- TODO split into multiple update functions


type Msg
    = Add
    | Delete Int
    | InputChange String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model | ingredients = { name = model.newIngredient.name, quantity = model.newIngredient.quantity, unit = model.newIngredient.unit } :: model.ingredients, newIngredient = { name = "", quantity = 1, unit = "" } }
                |> log "Added new ingredient"

        Delete index ->
            { model | ingredients = List.take index model.ingredients ++ List.drop (index + 1) model.ingredients }
                |> log "Deleted ingredient"

        InputChange name value ->
            updateNewIngredientProperty name value model


updateNewIngredientProperty : String -> String -> Model -> Model
updateNewIngredientProperty newName newValue model =
    case model.newIngredient of
        { name, quantity, unit } ->
            let
                updatedNewIngredient =
                    case newName of
                        "name" ->
                            { name = newValue, quantity = quantity, unit = unit }
                                |> log "Updated name"

                        "quantity" ->
                            { name = name, quantity = String.toInt newValue |> Maybe.withDefault 1, unit = unit }
                                |> log "Updated quantity"

                        "unit" ->
                            { name = name, quantity = quantity, unit = newValue }
                                |> log "Updated unit"

                        _ ->
                            model.newIngredient
            in
            { model | newIngredient = updatedNewIngredient }
                |> log "Updated new ingredient"



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "mx-auto max-w-7xl py-10" ]
        [ div []
            [ div [ class "sm:flex sm:items-center" ]
                [ div [ class "sm:flex-auto" ]
                    [ h1 [ class "text-base font-semibold leading-6 text-white" ]
                        [ text "Introduction" ]
                    , p [ class "mt-2 text-sm text-gray-300" ]
                        [ text "A list of all groceries I need to buy for this week." ]
                    ]
                , div [ class "mt-4 sm:ml-16 sm:mt-0 sm:flex-none" ]
                    [ div [ class "block rounded-md bg-indigo-500 px-3 py-2 text-center text-sm font-semibold text-white hover:bg-indigo-400 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-500", type_ "button" ]
                        [ text "Number of People: "
                        , input [ class "font-bold" ]
                            []
                        ]
                    ]
                ]
            , div [ class "mt-8 flow-root" ]
                [ div [ class "-mx-4 -my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" ]
                    [ div [ class "inline-block min-w-full py-2 align-middle sm:px-6 lg:px-8" ]
                        [ table [ class "min-w-full divide-y divide-gray-700" ]
                            [ thead []
                                [ tr []
                                    [ th [ class "w-3/4 py-3.5 pl-4 pr-3 text-left text-sm font-semibold text-white sm:pl-0", scope "col" ]
                                        [ text "Name" ]
                                    , th [ class "w-1/4 px-3 py-3.5 text-left text-sm font-semibold text-white", scope "col" ]
                                        [ text "Quantity" ]
                                    , th [ class "relative py-3.5 pl-3 pr-4 sm:pr-0", scope "col" ]
                                        [ span [ class "sr-only" ]
                                            [ text "Edit" ]
                                        ]
                                    ]
                                ]
                            , tbody [ class "divide-y divide-gray-800" ]
                                (List.indexedMap viewIngredient model.ingredients)
                            ]
                        ]
                    ]
                ]
            ]
        , div []
            [ viewForm model
            ]
        ]


viewIngredient : Int -> Ingredient -> Html Msg
viewIngredient index ingredient =
    tr [ id (String.fromInt index) ]
        [ td [ class "whitespace-nowrap py-4 pl-4 pr-3 text-sm font-medium text-white sm:pl-0" ]
            [ text ingredient.name ]
        , td [ class "whitespace-nowrap px-3 py-4 text-sm text-gray-300" ]
            [ span [ class "mr-1" ]
                [ text (String.fromInt ingredient.quantity)
                ]
            , span [] [ text ingredient.unit ]
            ]
        , td [ class "relative whitespace-nowrap py-4 pl-3 pr-4 text-right text-sm font-medium sm:pr-0" ]
            [ button [ class "text-red-400 hover:text-red-300", onClick (Delete index) ]
                [ text "Delete" ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    div [ class "mt-8" ]
        [ div [ class "mt-10 grid gap-x-6 gap-y-8 grid-cols-8" ]
            [ div [ class "col-span-3" ]
                [ label [ class "block text-sm font-medium leading-6 text-white", for "name" ]
                    [ text "Name" ]
                , div [ class "mt-2" ]
                    [ input [ attribute "autocomplete" "", class "block p-1 w-full rounded-md border-0 bg-white/5 py-1.5 text-white shadow-sm ring-1 ring-inset ring-white/10 focus:ring-2 focus:ring-inset focus:ring-indigo-500 sm:text-sm sm:leading-6", id "name", name "name", type_ "text", onInput (InputChange "name"), value model.newIngredient.name ]
                        []
                    ]
                ]
            , div [ class "col-span-3" ]
                [ label [ class "block text-sm font-medium leading-6 text-white", for "quantity" ]
                    [ text "Quantity" ]
                , div [ class "mt-2" ]
                    [ input [ attribute "autocomplete" "", class "block p-1 w-full rounded-md border-0 bg-white/5 py-1.5 text-white shadow-sm ring-1 ring-inset ring-white/10 focus:ring-2 focus:ring-inset focus:ring-indigo-500 sm:text-sm sm:leading-6", id "quantity", name "quantity", type_ "number", onInput (InputChange "quantity"), value (String.fromInt model.newIngredient.quantity) ]
                        []
                    ]
                ]
            , div [ class "col-span-2" ]
                [ label [ class " block text-sm font-medium leading-6 text-white", for "unit" ]
                    [ text "Unit" ]
                , div [ class "mt-2" ]
                    [ input [ attribute "autocomplete" "", class "block p-1 w-full rounded-md border-0 bg-white/5 py-1.5 text-white shadow-sm ring-1 ring-inset ring-white/10 focus:ring-2 focus:ring-inset focus:ring-indigo-500 sm:text-sm sm:leading-6", id "unit", name "unit", type_ "text", onInput (InputChange "unit"), value model.newIngredient.unit ]
                        []
                    ]
                ]
            ]
        , button [ class "bg-indigo-500 text-white font-semibold px-2 py-1 rounded-md mt-4", onClick Add ] [ text "Add" ]
        ]
