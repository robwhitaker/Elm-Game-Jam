module Data.AudioConstants exposing (..)

playerHits : List String
playerHits =
    [ "/assets/sfx/playerhit" ]

enemyHits : List String
enemyHits =
    [ "/assets/sfx/enemyhit"
    , "/assets/sfx/enemyhit-2"
    , "/assets/sfx/enemyhit-3"
    ]

fastSwordSwishes : List String
fastSwordSwishes =
    [ "/assets/sfx/swordswish"
    , "/assets/sfx/swordswish-2"
    , "/assets/sfx/swordswish-3"
    , "/assets/sfx/swordswish-4"
    , "/assets/sfx/swordswish-5"
    ]

slowSwordSwishes : List String
slowSwordSwishes =
    [ "/assets/sfx/swordswish-long"
    , "/assets/sfx/swordswish-long-2"
    ]

footsteps : List String
footsteps =
    [ "/assets/sfx/footstep"
    , "/assets/sfx/footstep-2"
    , "/assets/sfx/footstep-3"
    , "/assets/sfx/footstep-4"
    , "/assets/sfx/footstep-5"
    ]

bgm : String
bgm = "/assets/bgm/heavymetalsticks"

allSounds : List String
allSounds =
    List.concat
        [ playerHits, enemyHits, fastSwordSwishes, slowSwordSwishes, footsteps ]