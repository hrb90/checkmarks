module Distributions exposing (poisson, unsafePoisson)

import Random exposing (..)


-- Don't use this for large values of lambda


poissonHelper : Float -> Float -> Int -> Float -> Float -> Int
poissonHelper lambda seed counter cumulator p =
    let
        poissonHelper_ =
            poissonHelper lambda seed

        newP =
            p * lambda / (toFloat counter + 1.0)
    in
        if seed < cumulator then
            counter
        else
            poissonHelper_
                (counter + 1)
                (cumulator + newP)
                newP


uniformToPoisson : Float -> Float -> Int
uniformToPoisson lambda seed =
    let
        negExp =
            e ^ (-lambda)
    in
        poissonHelper lambda seed 0 negExp negExp


poisson : Float -> Maybe (Generator Int)
poisson lambda =
    if lambda < 0 then
        Nothing
    else
        Just (Random.map (uniformToPoisson lambda) (float 0 1))


unsafePoisson : Float -> Generator Int
unsafePoisson lambda =
    let
        p =
            poisson lambda
    in
        case p of
            Nothing ->
                (int 0 0)

            Just x ->
                x
