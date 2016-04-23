type 'a Tree = 
  EmptyTree
  | Node of 'a * 'a Tree * 'a Tree

let singleton x = Node (x, EmptyTree, EmptyTree)

let rec insert x = function
  EmptyTree -> singleton x
  | Node (y, l, r) ->
      if (x = y) then Node (y, l, r)
      elif (x < y) then Node (y, insert x l, r)
      else Node (y, l, insert x r)


let list2tree list = List.foldBack (fun x acc -> insert x acc) list EmptyTree

list2tree [2;7;4;3;5;8]

let foldTree treeFunction listValue tree =
    let rec loop tree cont =
        match tree with
        | EmptyTree -> cont listValue
        | Node (x, left, right) -> loop left (fun leftAcc -> 
            loop right (fun rightAcc -> 
              cont (treeFunction x leftAcc rightAcc)
            )
          )
    loop tree (fun x -> x)


let sumTree = foldTree (fun x left right -> x + left + right) 0
[2;7;4;3;5;8] |> list2tree |> sumTree


let heightTree = foldTree (fun _ left right -> 1 + max left right) 0
[2;7;4;3;5;8] |> list2tree |> heightTree 


let tree2List = foldTree (fun x left right -> left @ (x :: right)) []
[2;7;4;3;5;8] |> list2tree |> tree2List

