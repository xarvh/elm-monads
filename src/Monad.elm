module Monad exposing (..)

import Run

------------------------------------------------------------------------------

type Monad a
    = Success a
    | Error String

monadConstant : a -> Monad a
monadConstant a =
    Success a

monadAndThen : (a -> Monad b) -> Monad a -> Monad b
monadAndThen f ma =
    case ma of
        Success a -> f a
        Error s -> Error s

------------------------------------------------------------------------------

monadShow : Monad Value -> String
monadShow ma =
    case ma of
        Success a -> showValue a
        Error s -> "Error: " ++ s

------------------------------------------------------------------------------


(&>) = flip monadAndThen
infixl 9 &>




type alias VariableName =
    String

type Term
    = Variable VariableName
    | Constant Int
    | Addition Term Term
    | Lambda VariableName Term
    | FunctionCall Term Term

type Value
    = Wrong
    | Number Int
    | Function (Value -> Monad Value)

type alias Environment =
    List (VariableName, Value)

showValue : Value -> String
showValue v =
    case v of
        Wrong -> "<wrong>"
        Number n -> toString n
        Function f -> "<function>"

interprete : Environment -> Term -> Monad Value
interprete environment term =
    case term of
        Variable variableName ->
            lookup environment variableName

        Constant n ->
            monadConstant (Number n)

        Addition termA termB ->
            interprete environment termA &> \a ->
            interprete environment termB &> \b ->
            add a b

        Lambda variableName term ->
            monadConstant <| Function <| \x -> interprete ((variableName, x)::environment) term

        FunctionCall functionTerm argumentTerm ->
            interprete environment functionTerm &> \function ->
            interprete environment argumentTerm &> \argument ->
            apply function argument

lookup : Environment -> VariableName -> Monad Value
lookup environment variableName =
    case environment of
        [] ->
            Error ("Undefined variable `" ++ variableName ++ "`")

        (name, value) :: xs ->
            if name == variableName then monadConstant value else lookup xs variableName

add : Value -> Value -> Monad Value
add a b =
    case (a, b) of
        (Number aa, Number bb) -> monadConstant <| Number (aa + bb)
        -- TODO: add function addition =D
        _ -> Error ("both should be numbers: " ++ showValue a ++ " " ++ showValue b)

apply : Value -> Value -> Monad Value
apply a b =
    case a of
        Function f -> f b
        _ -> Error ("trying to call: `" ++ showValue a ++ "` but it is not a function =(")


test : Term -> String
test term =
    monadShow (interprete [] term)





------------------------------------------------------------------------------
test0 =
    test
        (FunctionCall
            (Lambda
                "x"
                (Addition
                    (Variable "x")
                    (Variable "x")
                )
            )
            (Addition
                (Constant 10)
                (Constant 11)
            )
        )

test1 =
    test
        (FunctionCall
            (Constant 1)
            (Constant 2)
        )

-- would be nice to have a parser
main =
    Run.once (\flags -> test1)
