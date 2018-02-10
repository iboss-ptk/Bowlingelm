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
frameRolls fuzzFirstRoll makeFuzzSecondRoll =
    fuzzFirstRoll
        |> Fuzz.map
            (\firstRoll ->
                { fuzzFirstRoll = Fuzz.constant firstRoll
                , fuzzSecondRoll = makeFuzzSecondRoll firstRoll
                }
            )
        |> Fuzz.andThen
            (\{ fuzzFirstRoll, fuzzSecondRoll } ->
                Fuzz.map2
                    (\firstRoll secondRoll ->
                        { firstRoll = { pinCount = firstRoll }
                        , secondRoll = { pinCount = secondRoll }
                        }
                    )
                    fuzzFirstRoll
                    fuzzSecondRoll
            )


invalidFirstRoll : Fuzzer RollsInNormalFrame
invalidFirstRoll =
    let
        fuzzFirstRoll =
            intExcludeRange minPinCount maxPinCount

        fuzzSecondRoll =
            \_ -> intRange minPinCount maxPinCount
    in
        frameRolls fuzzFirstRoll fuzzSecondRoll


invalidSecondRoll : Fuzzer RollsInNormalFrame
invalidSecondRoll =
    let
        fuzzFirstRoll =
            intRange minPinCount maxPinCount

        fuzzSecondRoll =
            \firstRoll -> intExcludeRange minPinCount (maxPinCount - firstRoll)
    in
        frameRolls fuzzFirstRoll fuzzSecondRoll


openFrameRolls : Fuzzer RollsInNormalFrame
openFrameRolls =
    let
        maxPinCountForOpenFrame =
            9

        fuzzFirstRoll =
            intRange minPinCount maxPinCountForOpenFrame

        fuzzSecondRoll =
            \firstRoll -> intRange 0 (maxPinCountForOpenFrame - firstRoll)
    in
        frameRolls fuzzFirstRoll fuzzSecondRoll


spareRolls : Fuzzer RollsInNormalFrame
spareRolls =
    let
        maxFirstPinCount =
            9

        fuzzFirstRoll =
            intRange minPinCount maxFirstPinCount

        fuzzSecondRoll =
            \firstRoll -> Fuzz.constant (maxPinCount - firstRoll)
    in
        frameRolls fuzzFirstRoll fuzzSecondRoll


strikeRolls : Fuzzer RollsInNormalFrame
strikeRolls =
    let
        fuzzFirstRoll =
            Fuzz.constant maxPinCount

        fuzzSecondRoll =
            \_ -> Fuzz.constant minPinCount
    in
        frameRolls fuzzFirstRoll fuzzSecondRoll


makeFrameTest : Test
makeFrameTest =
    describe "#makeFrame"
        [ describe "invalid frame"
            [ fuzz invalidFirstRoll "firstRoll is not in range [0, 10] " <|
                \rolls -> makeFrame rolls |> Expect.equal (Err "Invalid pin count")
            , fuzz invalidSecondRoll "secondRoll is not in range [0, 10 - firstRoll.pinCount]" <|
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
