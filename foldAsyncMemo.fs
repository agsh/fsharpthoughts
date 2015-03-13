(*
Сегодня речь пойдет о чрезвычайно общей и постоянно применяющейся в функциональном программировании технике - о свертках и префиксных суммах.
Мы изучим, что они собой представляют и какими бывают, увидим их многочисленные применения и поймем, зачем может быть полезно выразить алгоритм в виде свертки или префиксной сумме.

Рассмотрим вновь несколько рекурсивных функций обработки списков:
*)

let rec sum = function
  [] -> 0
  | h::t -> h + (sum t)

let rec mn = function
  [] -> 0
  | h::t -> h :: (mn t)

let rec concat = function
  [] -> []
  | h::t -> h @ (concat t)

let rec map f = function
  [] -> []
  | h::t -> (f h)::(map f t)

let rec filter f = function
  [] -> []
  | h::t when (f h) ->  h::(filter f t)
  | h::t -> filter f t

let rec any f = function
  [] -> false
  | h::t -> (f h) || (any f t)

let rec all f = function
  [] -> true
  | h::t -> (f h) && (any f t)

(*Все эти функции вычисляют формулу вида x1 # (x2 # (x3 # (... # u))
Например:
	sum:            +,      0
	min-list:       min,    0 (лучше бы -inf)
	concat:         append, ()
	map:            cons,   ()
	filter:         (\x r. if (p x) then (cons x r) else r), ()
	any:            (\x r. (p x) || r),         #f
	all:            (\x r. (p x) && r),         #t 
Формулы такого вида над списками называются "правыми свертками". 
А реализовать эту формулу можно так: *)

let rec foldr f a list =
  match list with
  [] -> a
  | h::t -> f h (foldr f a t)

let sum = foldr (+) 0
sum [1..6]

let filter f = foldr (fun x a -> if f x then x::a else a) []
filter (fun x -> x%2=0) [1..20]


// Теперь рассмотрим несколько итеративных функций:

let sum =
  let rec loop a = function
    [] -> a
    |h::t -> loop (h+a) t
  loop 0

let mn =
  let rec loop a = function
    [] -> a
    | h::t -> loop (h::a) t
  loop 0

let concat (list:'a list list) =
  let rec loop a = function
    [] -> a
    |h::t -> loop (a@h) t
  loop [] list

concat [[1..5];[6..7]]

let reverse (list:'a list) =
  let rec loop a = function
    [] -> a
    | h::t -> loop (h::a) t
  loop [] list
reverse [1..9]

(*
Все они вычисляют формулу вида

	(((u # x1) # x2) # .. ) # xn

Например:
  sum:            +,      0
	min:            min,    0 (лучше бы -inf)
	dl-concat:      dl-append, dl-empty
	reverse:        cons,   ()
	any:            (\r x. r || (p x)),         #f
	all:            (\r x. r && (p x)),         #t

  Формулы такого вида над списками называются "левыми свертками".
  Вот общая функция, вычисляющая левую свертку:
*)

let foldl f a list =
  let rec loop a = function
    [] -> a
    |h::t -> loop (f a h) t
  loop a list

let sum = foldl (+) 0

let reverse list = foldl (fun a x -> x::a) [] list
reverse [1..9]

(*
Эти две процедуры не просто реализуют один и тот же алгоритм ("свертку") двумя разными способами - это два разных алгоритма, предназначенных для разных целей.
Первый из них (левая свертка) - это итеративный алгоритм, начинающий с некоторого начального значения и модифицирующий его с помощью каждого элемента списка. В императивном языке ему соответствует цикл:

	Result res = u;
	for(Value v in list) {
	    res #= v;
	}
	return res;

Именно этот распространенный паттерн абстрагируется левой сверткой.
Он основан на том, что известно правило изменения ответа при дописывании элемента в конец списка.
Например:
	При дописывании элемента в конец списка его сумма увеличивается на этот элемент
	При дописывании элемента в конец списка к его reverse приписывается этот элемент

Итеративность левой свертки основана на том, что можно итеративно представить список в виде последовательности дописываний элемента в конец, начиная с пустого списка.
*)

// Хвостовая рекурсия

// Хвостовая рекурсия
let rec fact' = function
  0 -> 1
  | x -> x * fact' (x-1)
  
let rec fact'' x =
  if x <= 0I then 1I
  else x * fact'' (x-1I)

fact'' 1000000I

let fact x =
  let rec fac x acc = 
    if x <= 0I then acc
    else fac (x - 1I) (acc * x)
  fac x 1I

fact 1000000I

(*Правая же свертка - рекурсивный алгоритм, строящий составное значение из головы списка и ответа для хвоста списка.
В данном случае известно правило изменения ответа при приписывании элемента в начало списка.
Например:
	При приписывании элемента в начало списка его сумма увеличивается на этот элемент
	При приписывании элемента в начало списка списков к его конкатенации приписывается этот элемент

В общем случае простого и эффективного способа преобразования между этими двумя правилами не существует (нельзя легко понять, как изменяется результат от дописывания в конец, зная, как изменяется результат от дописывания в начало, и наоборот).*)

foldl (+) 0 [1..5]
foldl (-) 0 [1..5] // -15 = 0-1-2-3-4-5
foldr (-) 0 [1..5] // 3 = 1-(2-(3-(4-(5-0))))

List.foldBack (-) 0 [1..5]
(*
Однако в некоторых случаях результаты левой и правой свертки с одной и той же операцией и начальным значением - совпадают. В каких же?

	(((u # x1) # x2) # .. ) # xn
	x1 # (x2 # (x3 # (... # u)))

Записав эти формулы для списков из 1 или 3 элементов, получаем:

	forall x, u#x = x#u  - следовательно, во-первых, операция # должна оперировать над аргументами одинакового типа, а во-вторых, u должно коммутировать с каждым элементом этого типа.
	Отсюда НЕ следует, что u должно быть единицей для # - однако очень часто выбирают u именно таким образом. Рассмотрим именно этот случай.

	forall a b c, ((u#a)#b)#c = a#(b#(c#u)) --> (a#b)#c = a#(b#c) - т.е. операция # должна быть ассоциативна.

  Эти два условия достаточны (но, если u не единичный элемент, не необходимы) для того, чтобы результаты левой и правой свертки совпадали.

(вскоре мы увидим, что если функция выражается с *разной* операцией #, но с одинаковым значением u для левой и правой свертки, то имеет место гораздо более сильное свойство - Третья теорема о гомоморфизмах.)
 
 Помимо сверток над списками часто используются также "бегущие свертки" (scan), называемые "префиксными суммами": например, "бегущая сумма", "бегущий минимум". Их можно интерпретировать как вычсления последовательности промежуточных результатов свертки.
   *)

List.scanBack (+) [1..5] 0
List.scan (+) 0 [1..5]

// Теперь вспомним пару функций над деревьями:

type 'a Tree =
  Node of 'a * 'a Tree * 'a Tree 
  | Leaf 

let tree = Node(4, Node(2,Node(1, Leaf,Leaf),Node(3,Leaf,Leaf)),Node(6,Node(5, Leaf,Leaf),Node(7,Leaf,Leaf)))

let rec sumTree tree =
  match tree with
  Node(v, ltree, rtree) -> v + (sumTree ltree) + (sumTree rtree)
  | Leaf -> 0 

sumTree tree

let rec to_list tree =
  match tree with
  Node(v, ltree, rtree) -> (to_list ltree)@[v]@(to_list rtree)
  | Leaf -> [] 

let FoldTree treeF leafV tree =
  let rec loop tree cont =
     match tree with
     Node (vl, left, right) -> loop left (fun lacc -> loop right (fun racc -> cont (treeF vl lacc racc)))
     |Leaf -> cont leafV
  loop tree (fun x -> x) 

to_list tree

let SumTree = FoldTree (fun x left right -> x + left + right) 0

let HeightTree = FoldTree (fun _ left right -> 1 + max left right) 0

let Tree2List = FoldTree (fun x left right -> left @ [x] @ right)

(* В общем случае, операцию свертки можно аналогичным образом определить для любой древовидной структуры. Списки тоже являются частным случаем такой структуры - это просто деревья с коэффициентом ветвления, равным 1 (у дерева может быть 0 или 1 ребенок). 
Любопытно, что свертка очень похожа на доказательство по индукции: чтобы вычислить доказательство свойства для всей структуры, надо доказать его для ее базовых случаев (например, для пустого списка), и привести способ вычислить доказательство для сложной структуры из доказательств для ее частей.
Можно сказать, что свертка - это "вычисление по индукции".
*)

(*Нам понадобится понятие моноида.

Моноидом называется тройка (M, #, u), где # - ассоциативная бинарная операция на M, а u - ее единичный элемент.
И в программировании, и в математике моноиды встречаются на каждом шагу. Например: 
 - (int, +, 0)
 - (bool, &&, true)
 - (bool, ||, false)
 - (M, min, bottom), где M - вполне упорядоченное множество, а bottom - его минимальный элемент
 - (списки, append, пустой список)
 - (множества, объединение, пустое множество)
 - (сортированные списки, merge, пустой список)
 - (матрицы, умножение, единичная матрица)
 - (невырожденные матрицы, умножение, единичная матрица)
 - (эндоморфизмы, композиция, тождественная функция), где эндоморфизмом на множестве M называется функция из M в M
 - (перестановки, умножение, тождественная перестановка)
 - (блоки кода, последовательное выполнение, пустой блок кода)
*)

(*
Нам понадобится понятие моноида.

Моноидом называется тройка (M, #, u), где # - ассоциативная бинарная операция на M, а u - ее единичный элемент.
И в программировании, и в математике моноиды встречаются на каждом шагу. Например: 
 - (int, +, 0)
 - (bool, &&, true)
 - (bool, ||, false)
 - (M, min, bottom), где M - вполне упорядоченное множество, а bottom - его минимальный элемент
 - (списки, append, пустой список)
 - (множества, объединение, пустое множество)
 - (сортированные списки, merge, пустой список)
 - (матрицы, умножение, единичная матрица)
 - (невырожденные матрицы, умножение, единичная матрица)
 - (эндоморфизмы, композиция, тождественная функция), где эндоморфизмом на множестве M называется функция из M в M
 - (перестановки, умножение, тождественная перестановка)
 - (блоки кода, последовательное выполнение, пустой блок кода)
 *)

 (*
Eще одно важное понятие - гомоморфизм моноидов.
Если (M, #, e) и (P, @, u) - моноиды, то функция f :: M -> P называется гомоморфизмом между этими двумя моноидами, если f (m1 # m2) = f m1 @ f m2 для всех m1,m2 из M. Легко показать, что в этом случае верно и f e = u.

Теперь перейдем к сверткам.
Списочным гомоморфизмом называется гомоморфизм из моноида (списки, append, пустой список) в какой-либо другой моноид.

То есть, функция f называется списочным гомоморфизмом, если существует оператор #, такой, что f (xs ++ ys) = f xs # f ys. Это свойство позволяет независимо вычислить результаты применения функции для подсписков, и собрать из них результат для всего списка при помощи #.

Чтобы задать такую функцию, достаточно указать u, # и значение функции на одноэлементных списках "m" - поскольку с помощью этих значений и операций можно вычислить значение функции над любым списком.

(define (homo @ m u xs)
  (if (null? xs)
	  u
	  (@ (m (car xs))
	     (homo @ m u (cdr xs)))))

Например:
 - sum.      @ = +,      u = 0,  m = \x . x
 - length.   @ = +,      u = 0,  m = \x . 1
 - filter p. @ = append, u = [], m = \x . if (p x) then [x] else []
 - map f.    @ = append, u = [], m = \x . [f x]
 - sort.     @ = merge,  u = [], m = \x . [x]

Чрезвычайно важно, что благодаря ассоциативности @, в выражении x1 @ x2 @ x3 @ ... @ xn можно расставлять скобки как угодно, вычисляя его в любом порядке (надо, однако, помнить, что @ не обязан быть коммутативным). Вышенаписанная программа вычисляет это выражение справа налево.


С практической точки зрения очень важен другой способ его вычисления - с помощью дерева:

	         (X)
	        ...
	   (X)
	  /  \
   (X)    (X)         (X)
   / \    / \         / \
 x1  x2  x3  ..    xn-1  xn

Он важен тем, что:
1) Позволяет вычислить ответ для N элементов за log N фаз (соответствующих уровням дерева), каждая из которых может быть распараллелена. На P процессорах можно ускорить программу в O(P/logP) раз.
2) Позволяет при изменении значения какого-нибудь элемента перевычислить ответ для всего списка за O(log N) операций, изменив только элементы по пути от измененного к корню.

*)


let rec fib x = if x <= 2I then 1I else fib(x-1I) + fib(x-2I)

fib 5I
(*
let u =
  let rec l x i = seq {yield x; yield! l (x+i) ((-1)*((abs i)+1))}
  l 1 1
Seq.take 6 u

Seq.unfold (fun (x,y) -> Some(x, (x+y, ((-1)*((abs y)+1)) ))) (1,1) |> Seq.take 6
*)


let duration f = 
  let sw = new System.Diagnostics.Stopwatch()
  sw.Start()
  let v = f()
  sw.Stop()
  printf "%A" sw.ElapsedMilliseconds
  v
  
// одно ядро
duration(fun() -> 
  [for i in 0..7 -> fib 32I]
  |> ignore
  )
// много ядер
let fibA x = async {
  return fib x
}

duration (fun () ->
  [ for i in 0..7 -> fibA 32I ]
  |> Async.Parallel 
  |> Async.RunSynchronously
  |> ignore
  )

let countSAS x =
  printf "%A\t" x
  duration(fun() -> 
    [for i in 0..7 -> fib x]
    |> ignore
    )
  printf "\t"
  duration (fun () ->
    [ for i in 0..7 -> fibA x]
    |> Async.Parallel 
    |> Async.RunSynchronously
    |> ignore
  )
  printf "\n"

countSAS 30I

for i in [25I..30I] do countSAS i
(*
30	4112L	  1040L
31	6697L	  1657L
32	10651L	2616L
33	17321L	4204L
34	28179L	6777L
35	45293L	10971L
36	72539L	17768L
*)

// загрузка 4-х из 8-ми ядер
let fibs2 =
  [| for i in 0..3 do yield! [|1I; 34I|] |]
  |> Array.map (fun x -> fibA x)
  |> Async.Parallel
  |> Async.RunSynchronously

// своя реализация map
let pmap f l =
  seq {for i in l -> async {return f i}}
  |> Async.Parallel
  |> Async.RunSynchronously

let rec fibs x = if x <= 2 then 1 else fibs(x-1) + fibs(x-2)

// основана на PLINQ
// предыдущая - создаёт 100500 заданий, а потом стартует
open Microsoft.FSharp.Collections
// #r "C:\Program Files (x86)\FSharpPowerPack-2.0.0.0\\bin\FSharp.PowerPack.Parallel.Seq.dll";;


// большие числа
duration (fun() -> pmap fibs [40..44]) |> printf "\nFibsAsync: %A"
duration (fun() -> List.map fibs [40..44]) |> printf "\nFibsSync: %A"
duration (fun() -> [40..44] |> PSeq.map fibs |> PSeq.toList) |> printf "\nFibsPsync: %A"
// много маленьких чисел
let manymany = List.init 30000 (fun _ -> 5)
duration (fun() -> pmap fibs manymany) |> printf "\nFibsAsync: %A"
duration (fun() -> List.map fibs manymany) |> printf "\nFibsSync: %A"
duration (fun() -> manymany |> PSeq.map fibs |> PSeq.toList) |> printf "\nFibsPsync: %A"



[1..10000] |> List.filter (fun n -> List.filter (fun x -> n%x=0) [1..n/2] |> List.sum = n)

//6 = 1+2+3
//28 = 1+2+4+7+14


let dividers n = List.filter (fun x -> n%x=0) [1..n/2]
let perfect n = dividers n |> List.sum = n
duration (fun _ -> [1..10000] |> List.filter perfect) |> printf "\n%A"  // 2700
//System.Int16.MaxValue
duration (fun() -> [1..35000000] |> List.filter perfect) |> printf "%A" // undefined

let pdividers n = seq {1..n/2} |> PSeq.filter (fun x -> n%x=0)
let pperfect n = pdividers n |> PSeq.sum = n
duration (fun _ -> [1..10000] |> PSeq.filter pperfect |> PSeq.toList) |> printf "\n%A" // 1700
duration (fun _ -> [1..35000000] |> PSeq.filter pperfect |> PSeq.toList) |> printf "\n%A" // undefined


// не всё можно решить параллельностью

let sqrtInt (n:int) = (int)(System.Math.Sqrt((float)n))

let testDiv n acc x =
  if (n%x=0) then 
    let y = n/x
    if (x=y) then x::acc else x::y::acc
  else acc

let dividers2 n = (List.fold (fun a x -> testDiv n a x) [1] [2..(sqrtInt n)])
let perfect2 n = dividers2 n |> List.sum = n
duration (fun _ -> [2..10000] |> List.filter perfect2) |> printf "\n%A"  // 90
duration (fun _ -> [2..35000000] |> List.filter perfect2) |> printf "\n%A"// всю ночь, UNDEFINED

// декларативный язык прячет реализацию

let pdividers2 n = seq {2..(sqrtInt n)} |> PSeq.fold (fun a x -> testDiv n a x) [1]
let pperfect2 n =
  if (n%10000=0) then printfn "%A" n
  pdividers2 n |> PSeq.sum = n

duration (fun _ -> [2..10000] |> PSeq.filter pperfect2 |> PSeq.toList) |> printf "\n%A" // 200
duration (fun _ -> [2..35000000] |> PSeq.filter pperfect2 |> PSeq.toList) |> printf "\n%A" // полтора часа

(*
let Mperfect upto =
  let rec mperfect n primes acc =
    let dividersSum = primes |> List.filter (fun x -> n%x=0) |> List.sum
    match dividersSum with    
    | 1 -> printfn "%A" primes; mperfect (n+1) (n::primes) acc
    | x when x = n -> mperfect (n+1) primes (n::acc)
    | x when n >= upto -> acc
    | _ -> mperfect (n+1) primes acc
  mperfect 4 [3; 2; 1] []

Mperfect 100
duration (fun _ -> Mperfect) |> printf "\n%A"  // 2700  
*)





let mutable yarr = 0
// распараллеливание и мемоизация
let rnd = new System.Random()
let M = 100
let N = 100
let matrix m n =
  List.init n (fun x -> List.init m (fun y -> rnd.Next(1,100)))
let mtrx = matrix M N

type Cells = (int * int) list
type Memory = int * Cells

let cellValue (x,y) = 
  List.nth (List.nth mtrx (y-1)) (x-1)
cellValue (10,9)

let inRange (x,y) =
  x>0 && y>0 && x<=M && y<=N

let rec getCells x y =
  //yarr <- yarr + 1
  (y,x)
  ::
  [for i in [(x+1,y);(x-1,y);(x,y-1);(x,y+1)] do
    if (inRange i && cellValue i < cellValue (x,y)) then
      yield! getCells (fst i) (snd i)
  ]

let inCells cell cells =
  List.exists (fun i -> i=cell) cells

let removeDublicates (cells:Cells) =
  List.fold (fun a x -> if inCells x a then a else x::a) [] cells

// вывод на экран


  
let printMtrix cells =
  List.iteri (fun row list -> List.iteri (fun col vl -> if inCells (row+1,col+1) cells then printf "[%d\t" vl else printf "%d\t" vl) list; printf "\n") mtrx

printMtrix [(1,3)]
printMtrix (getCells 3 2)

open System.Collections.Generic


let slv = 
  [for x in [1..M] do
    for y in [1..N] ->
      (*
      async {return List.length(getCells x y), (x,y)}]
      |> Async.Parallel
      |> Async.RunSynchronously
      |> Array.toList
      *)
      (List.length(getCells x y), (x,y))]
  |> List.fold (fun a x -> if fst x > fst a then x else a) (0, (0,0))
  |> fun (res,(x,y)) -> printfn "%d : (%d,%d)" res x y; printMtrix (getCells x y)
yarr
getCells 4 6

let d = new Dictionary<(int * int), Cells>()


let rec getCells x y =
  yarr <- yarr + 1
  if d.ContainsKey(x,y) then d.[(x,y)]
  else
    let res =
      (y,x)
      ::
      [for i in [(x+1,y);(x-1,y);(x,y-1);(x,y+1)] do
        if (inRange i && cellValue i < cellValue (x,y)) then
          yield! getCells (fst i) (snd i)
      ]
    d.Add((x,y), res)
    res
    
yarr <- 0
yarr

removeDublicates (getCells 4 6)

