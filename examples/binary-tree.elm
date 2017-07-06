{- OVERVIEW ------------------------------------------------------

A "Tree" represents a binary tree. A "Node" in a binary tree
always has two children. A tree can also be "Empty". Below I have
defined "Tree" and a number of useful functions.

This example also includes some challenge problems!

-----------------------------------------------------------------}


import Html exposing (Html, div, text, h1)
import Html.Attributes exposing (style)



-- TREES


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton v =
    Node v Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
      Empty ->
          singleton x

      Node y left right ->
          if x > y then
              Node y left (insert x right)

          else if x < y then
              Node y (insert x left) right

          else
              tree


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
      Empty -> 0
      Node v left right ->
          1 + max (depth left) (depth right)


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
      Empty -> Empty
      Node v left right ->
          Node (f v) (map f left) (map f right)

sum : Tree number -> number
sum tree =
    case tree of
        Empty -> 0
        Node value left right ->
            value + sum left + sum right

flatten : Tree a -> List a
flatten tree =
    case tree of
        Empty -> []
        Node value left right ->
            [value] ++ flatten left ++ flatten right

isElement : a -> Tree a -> Bool
isElement value tree =
    case tree of
        Empty -> False
        Node treeValue left right ->
            if value == treeValue then
                True
            else
                isElement value left || isElement value right


-- PLAYGROUND


deepTree =
  fromList [1,2,3]


niceTree =
  fromList [2,1,3]

randomTree =
    fromList [5, 1, 4, 7, 6 ,8, 2]

main =
  div [ style [ ("font-family", "monospace") ] ]
    [ h1 [] [text "Nice tree"]
    , display "depth niceTree" (depth niceTree)
    , display "sum niceTree" (sum niceTree)
    , display "incremented niceTree" (map (\n -> n + 1) niceTree)
    , display "flatten niceTree" (flatten niceTree)
    , h1 [] [text "Deep tree"]
    , display "depth deepTree" (depth deepTree)
    , display "sum deepTree" (sum deepTree)
    , display "flatten deepTree" (flatten deepTree)
    , h1 [] [text "Random tree"]
    , display "randomTree" (randomTree)
    , display "depth randomTree" (depth randomTree)
    , display "sum randomTree" (sum randomTree)
    , display "flatten randomTree" (flatten randomTree)
    , display "isElement 7" (isElement 7 randomTree)
    , display "isElement 2" (isElement 2 randomTree)
    , display "isElement 7" (isElement 3 randomTree)
    , display "isElement 42" (isElement 42 randomTree)
    ]


display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]



{-----------------------------------------------------------------

Exercises:

(1) Sum all of the elements of a tree.

       sum : Tree number -> number

(2) Flatten a tree into a list.

       flatten : Tree a -> List a

(3) Check to see if an element is in a given tree.

       isElement : a -> Tree a -> Bool

(4) Write a general fold function that acts on trees. The fold
    function does not need to guarantee a particular order of
    traversal.

       fold : (a -> b -> b) -> b -> Tree a -> b

(5) Use "fold" to do exercises 1-3 in one line each. The best
    readable versions I have come up have the following length
    in characters including spaces and function name:
      sum: 16
      flatten: 21
      isElement: 46
    See if you can match or beat me! Don't forget about currying
    and partial application!

(6) Can "fold" be used to implement "map" or "depth"?

(7) Try experimenting with different ways to traverse a
    tree: pre-order, in-order, post-order, depth-first, etc.
    More info at: http://en.wikipedia.org/wiki/Tree_traversal

-----------------------------------------------------------------}




