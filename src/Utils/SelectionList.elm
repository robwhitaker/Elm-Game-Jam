module Utils.SelectionList exposing (..)

type alias SelectionList a =
    { previous : List a
    , selected : a
    , next : List a
    , index : Int
    }

fromList : a -> List a -> SelectionList a
fromList head rest =
    { previous = []
    , selected = head
    , next = rest
    , index = 0
    }

next : SelectionList a -> SelectionList a
next sl =
    case List.head sl.next of
        Nothing -> sl
        Just n ->
            { sl
                | previous = sl.selected :: sl.previous
                , selected = n
                , next = List.drop 1 sl.next
                , index = sl.index + 1
            }

previous : SelectionList a -> SelectionList a
previous sl =
    case List.head sl.previous of
        Nothing -> sl
        Just p ->
            { sl
                | previous = List.drop 1 sl.previous
                , selected = p
                , next = sl.selected :: sl.next
                , index = sl.index - 1
            }

goto : Int -> SelectionList a -> SelectionList a
goto index sl =
    let diff = index - sl.index
        fn = if diff < 0 then previous else next
    in
        if diff /= 0 && (fn sl).index /= sl.index
            then goto index (fn sl)
            else sl

selected : SelectionList a -> a
selected = .selected