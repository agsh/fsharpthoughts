let rec cc amount coins =
  if amount = 0
  then 1
  else 
    if (amount < 0 || List.length coins = 0)
    then 0
    else cc amount (List.tail coins) + cc (amount - List.head coins) coins

cc 100 (List.rev [50; 25; 10; 5; 1])

type Anniversary =
 Birthday of string * int * int * int
 | Wedding of string * string * int * int * int
 | Death of string * int * int * int

Birthday ("someone", 2014, 5, 4)
// Birthday "someone" 2012 11 7
let today = Birthday ("someone", 2014, 5, 4)
// today
// Birthday "someone" 2012 11 7


let (kurtCobain : Anniversary) = Birthday ("Kurt Cobain", 1967, 2, 20)
let (kurtWedding : Anniversary) = Wedding ("Kurt Cobain", "Courtney Love", 1990, 1 ,12)
let anniversaries = [
    kurtCobain;
    kurtWedding;
    Death ("Kurt Cobain", 1994, 4, 5)
]

let showDate (y: int) m d = y.ToString() + "." + m.ToString() + "." + d.ToString()

let showAnniversary = function
  Birthday (name, year, month, day) -> name + " born " + showDate year month day // синеньким
  | Wedding (name1, name2, year, month, day) ->
      name1 + " married " + name2 + " on " + showDate year month day
  | Death (name, year, month, day) -> name + " dead in " + showDate year month day

let who = function
 Birthday (him, _, _, _) -> him 
 | Wedding (him, _, _, _, _) -> him
 | Death (him, _, _, _) -> him

List.map who anniversaries
(*
1) Kurt Cobain born 1967-2-20
2) Kurt Cobain married Courtney Love on 1990-1-12
3) Kurt Cobain dead 1994-4-5
*)

type Point = { x  : float; y : float }

let a = { x = 13.22 ; y = 8.99 }
let b = { a with y = 666.13 }
let absPoint a = sqrt (a.x*a.x + a.y*a.y)

Some 1
None
Some "str" // type?
Some 42
Some 42 :: [None]  // ?
Some 42 :: [Some "str", None] // ?

type Option<'a> =
  Some of 'a
  | None

type 'a List =  // haskell!!!
  Nil
  | Cons of 'a * ('a List)

let l1 = Cons (3, (Cons (4, (Cons (5, Nil)))))

let rec apply x y = 
  match x with
    | Nil -> y
    | Cons (head, tail) -> Cons (head, apply tail y)

apply l1 (Cons (1, Nil))

type 'a Tree =
  EmptyTree
  | Node of 'a * 'a Tree * 'a Tree

let singleton x = Node (x, EmptyTree, EmptyTree)

let rec treeInsert x = function
  EmptyTree -> singleton x 
  | Node (a, left, right) -> 
    if x = a then Node (x, left, right) 
    else 
      if x < a then Node (a, (treeInsert x left), right) 
      else Node (a, left, (treeInsert x right))
// when 'a : comparsion

let list2tree list =
 let rec l2t acc = function
   [] -> acc
   | (head::tail) -> l2t (treeInsert head acc) tail
 in l2t EmptyTree list

list2tree [12; 1; 6; 4; 90; 9]

tree2list // сами

let rec tree2list = function
  EmptyTree -> []
  | Node (v, left, right) -> List.append (tree2list left) (v :: tree2list right)

let treesort x = x |> list2tree |> tree2list

treesort [12; 1; 6; 4; 90; 9]

list2tree [12; 12; 12; 13; 13; 14]
Как будет выглядеть дерево?


type 'a Tree =
  EmptyTree
  | Node of 'a * int * 'a Tree * 'a Tree



let singleton x = Node (x, 1, EmptyTree, EmptyTree)

let rec treeInsert x = function
 | EmptyTree -> singleton x
 | Node (a, i, left, right) when x = a -> Node (x, (i+1), left, right)
 | Node (a, i, left, right) when x < a -> Node (a, i, (treeInsert x left), right)
 | Node (a, i, left, right) when x > a -> Node (a, i, left, (treeInsert x right))

let list2tree x = 
  let rec l2t acc = function
    | [] -> acc
    | (head::tail) -> l2t (treeInsert head acc) tail
  l2t EmptyTree x

// tree2list - сами
