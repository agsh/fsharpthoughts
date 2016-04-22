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
let c = singletonSet 6
contains b 5
contains b 6

let union (a:Set) (b:Set) =  fun c -> (contains b c) || (contains a c)
let d = union b c
contains d 5
contains d 4

let intersect (a: Set) (b: Set) = fun c -> (contains b c) && (contains a c)


let e = intersect d c
contains e 5
contains e 6

let diff (a:Set) (b:Set) = fun c -> (contains a c) && (not (contains b c))


let filter a f = fun c -> (contains a c) && (f c)
let f = filter d (fun x -> x % 2 = 0)


let forAll (a: Set) (f: int -> bool) =
  let rec forAll' = function
    1000 -> true
    | acc -> printf "%d" acc; if (contains a acc) && not (f acc) then false else forAll' (acc+1)
  forAll' (-1000)

forAll f (fun x -> x = 6)


let exists (a: Set) (f: int -> bool) = not (forAll a (fun x -> not (f x)))
exists d (fun x -> x = 5)
exists d (fun x -> x = 4)
