type Set = int -> bool


let (a:Set) = (fun a -> true)

let contains (s:Set) (a:int) = s a
contains a 1

let singletonSet b = fun a -> a = b
let singletonSet (b:int) = fun (a:int) -> a = b
let singletonSet b = fun (a:int) -> a = b
let singletonSet (b:int) = fun a -> a = b
let singletonSet = fun b -> fun (a: int) -> a = b
let singletonSet b =
  let answer a = a=b
  in (answer:Set)

let b = singletonSet 5
contains b 5
contains b 6
