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
    { roll1 : Roll, roll2 : Roll }


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
                { roll1 = { pinCount = 0 }
                , roll2 = { pinCount = 0 }
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
makeFrame { roll1, roll2 } =
    let
        isRoll1Valid =
            roll1.pinCount >= minPinCount && roll1.pinCount <= maxPinCount

        isRoll2Valid =
            roll2.pinCount >= minPinCount && roll2.pinCount <= (maxPinCount - roll1.pinCount)
    in
        if not (isRoll1Valid && isRoll2Valid) then
            Err "Invalid pin count"
        else if roll1.pinCount == maxPinCount then
            Ok <| Strike
        else if roll1.pinCount + roll2.pinCount == maxPinCount then
            Ok <| Spare { roll1 = roll1, roll2 = roll2 }
        else
            Ok <| OpenFrame { roll1 = roll1, roll2 = roll2 }
