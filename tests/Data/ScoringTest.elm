module Data.ScoringTest exposing (..)

import Fuzz exposing (Fuzzer, intRange, oneOf)
import Test exposing (..)
import Data.Scoring exposing (..)
import Expect
import Random


intExcludeRange : Int -> Int -> Fuzzer Int
intExcludeRange lo hi =
    oneOf
        [ intRange Random.minInt (lo - 1)
        , intRange (hi + 1) Random.maxInt
        ]


frameRolls : Fuzzer Int -> (Int -> Fuzzer Int) -> Fuzzer RollsInNormalFrame
frameRolls fuzzRoll1 makeFuzzRoll2 =
    fuzzRoll1
        |> Fuzz.map
            (\roll1 ->
                { fuzzRoll1 = Fuzz.constant roll1
                , fuzzRoll2 = makeFuzzRoll2 roll1
                }
            )
        |> Fuzz.andThen
            (\{ fuzzRoll1, fuzzRoll2 } ->
                Fuzz.map2
                    (\roll1 roll2 ->
                        { roll1 = { pinCount = roll1 }
                        , roll2 = { pinCount = roll2 }
                        }
                    )
                    fuzzRoll1
                    fuzzRoll2
            )


invalidFirstRoll : Fuzzer RollsInNormalFrame
invalidFirstRoll =
    let
        fuzzRoll1 =
            intExcludeRange minPinCount maxPinCount

        fuzzRoll2 =
            \_ -> intRange minPinCount maxPinCount
    in
        frameRolls fuzzRoll1 fuzzRoll2


invalidSecondRoll : Fuzzer RollsInNormalFrame
invalidSecondRoll =
    let
        fuzzRoll1 =
            intRange minPinCount maxPinCount

        fuzzRoll2 =
            \roll1 -> intExcludeRange minPinCount (maxPinCount - roll1)
    in
        frameRolls fuzzRoll1 fuzzRoll2


openFrameRolls : Fuzzer RollsInNormalFrame
openFrameRolls =
    let
        maxPinCountForOpenFrame =
            9

        fuzzRoll1 =
            intRange minPinCount maxPinCountForOpenFrame

        fuzzRoll2 =
            \roll1 -> intRange 0 (maxPinCountForOpenFrame - roll1)
    in
        frameRolls fuzzRoll1 fuzzRoll2


spareRolls : Fuzzer RollsInNormalFrame
spareRolls =
    let
        maxFirstPinCount =
            9

        fuzzRoll1 =
            intRange minPinCount maxFirstPinCount

        fuzzRoll2 =
            \roll1 -> Fuzz.constant (maxPinCount - roll1)
    in
        frameRolls fuzzRoll1 fuzzRoll2


strikeRolls : Fuzzer RollsInNormalFrame
strikeRolls =
    let
        fuzzRoll1 =
            Fuzz.constant maxPinCount

        fuzzRoll2 =
            \_ -> Fuzz.constant minPinCount
    in
        frameRolls fuzzRoll1 fuzzRoll2


scoring : Test
scoring =
    describe "#makeFrame"
        [ describe "invalid frame"
            [ fuzz invalidFirstRoll "roll1 is not in range [0, 10] " <|
                \rolls -> makeFrame rolls |> Expect.equal (Err "Invalid pin count")
            , fuzz invalidSecondRoll "roll2 is not in range [0, 10 - roll1.pinCount]" <|
                \rolls -> makeFrame rolls |> Expect.equal (Err "Invalid pin count")
            ]
        , describe "valid frame"
            [ fuzz openFrameRolls "returns OpenFrame when total rolls < 10" <|
                \rolls -> makeFrame rolls |> Expect.equal (Ok (OpenFrame rolls))
            , fuzz spareRolls "returns Spare frame when total pin count is 10 but first roll <= 9" <|
                \rolls -> makeFrame rolls |> Expect.equal (Ok (Spare rolls))
            , fuzz strikeRolls "returns Strike when first roll is 10" <|
                \rolls -> makeFrame rolls |> Expect.equal (Ok Strike)
            ]
        ]
