module Data.Scoring
    exposing
        ( RollsInNormalFrame
        , Roll
        , NormalFrame(..)
        , makeFrame
        , maxPinCount
        , minPinCount
        )


type alias ErrMsg =
    String


type alias Roll =
    { pinCount : Int }


type alias RollsInNormalFrame =
    { firstRoll : Roll, secondRoll : Roll }


type NormalFrame
    = OpenFrame RollsInNormalFrame
    | Spare RollsInNormalFrame
    | Strike


type Bonus
    = FromStrike Roll Roll
    | FromSpare Roll


type Frame
    = NormalFrame NormalFrame
    | LastFrame { frame : NormalFrame, bonus : Maybe Bonus }


type alias FrameWithContext =
    { frame : Frame
    , frameCount : Int
    , leftOverRoll : Maybe Roll
    }


emptyFrame : FrameWithContext
emptyFrame =
    { frame =
        NormalFrame
            (OpenFrame
                { firstRoll = { pinCount = 0 }
                , secondRoll = { pinCount = 0 }
                }
            )
    , frameCount = 0
    , leftOverRoll = Nothing
    }


maxPinCount : Int
maxPinCount =
    10


minPinCount : Int
minPinCount =
    0


makeFrame : RollsInNormalFrame -> Result ErrMsg NormalFrame
makeFrame { firstRoll, secondRoll } =
    let
        isFirstRollValid =
            firstRoll.pinCount >= minPinCount && firstRoll.pinCount <= maxPinCount

        isSecondRollValid =
            secondRoll.pinCount >= minPinCount && secondRoll.pinCount <= (maxPinCount - firstRoll.pinCount)
    in
        if not (isFirstRollValid && isSecondRollValid) then
            Err "Invalid pin count"
        else if firstRoll.pinCount == maxPinCount then
            Ok <| Strike
        else if firstRoll.pinCount + secondRoll.pinCount == maxPinCount then
            Ok <| Spare { firstRoll = firstRoll, secondRoll = secondRoll }
        else
            Ok <| OpenFrame { firstRoll = firstRoll, secondRoll = secondRoll }


performRoll : FrameWithContext -> Roll -> Result ErrMsg FrameWithContext
performRoll frameWC newRoll =
    Ok emptyFrame
