(* Генерация списков *)
let list1 = [1 .. 10]       // val it : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let list2 = [1 .. 2 .. 10]  // val it : int list = [1; 3; 5; 7; 9]
let list3 = ['a' .. 'g']    // val it : char list ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g';]

// Ключевая конструкция yield или -> для генерации списков
let list4 = [ for a in 1 .. 10 do yield (a * a) ] // val it : int list = [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]

// Несколько элементов
let list5 = 
  [ for a in 1 .. 3 do
      for b in 3 .. 7 do
        yield (a, b) ]
// val it : (int * int) list = [(1, 3); (1, 4); (1, 5); (1, 6); (1, 7); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7); (3, 3); (3, 4); (3, 5); (3, 6); (3, 7)]

// Синтаксический сахар для замены do-yield: ->
let list6 = 
  [for a in 1..3 do
    for b in 4..6 ->
      (a, b) ]

// генерация списка с условием
let list7 = [ for a in 1 .. 100 do 
                if a % 3 = 0 && a % 5 = 1 then yield a]
// val it : int list = [6; 21; 36; 51; 66; 81; 96]

// для любых перечислимых типов
let list8 = [for a in ['a'.. 'f'] do yield [a; a; a] ]
// val it : char list list = [['a'; 'a'; 'a']; ['b'; 'b'; 'b']; ['c'; 'c'; 'c']; ['d'; 'd'; 'd']; ['e'; 'e'; 'e']; ['f'; 'f'; 'f']]

// yield! используется для генерации одновременно нескольких элементов
let list9 = [for a in 1 .. 5 do yield! [ a .. a + 3 ] ] // val it : int list = [1; 2; 3; 4; 2; 3; 4; 5; 3; 4; 5; 6; 4; 5; 6; 7; 5; 6; 7; 8]

// для генерации списка можно использовать различные возможности языка
let list10 = 
  [
    let thisIsIt = "!"
    for a in 1 .. 5 do
      match a with
      | 3 -> yield! ["hello"; "world"; thisIsIt]
      | _ -> yield a.ToString()
  ]
// val it : string list = ["1"; "2"; "hello"; "world"; "!"; "4"; "5"]


// последовательности задаются похожим на списки образом
let seq1 = seq {1..78}
seq1 |> Seq.iter (printfn "%d")
// если нужно задать последовательность перечислением элементов, используются квадратные скобки
let seq2 = seq [1; 2; 9]
// последовательность может быть сколь угодно большой, т.к. элементы последовательности высчитываются только тогда, когда они требуются
let seq3 = seq { 1I .. 1000000000000I }
let list11 = [1I .. 1000000000000I ]

// ещё один пример
let seq4 =
    seq { for a in 1 .. 10 do
            printfn "created: %i" a
            yield a }
// val seq4 : seq<int>, 
seq4 |> Seq.take 3
(*
а вот когда нам нужен 3-ий элемент последовательности, начинаются вычисления
created: 1
created: 2
created: 3
val it : seq<int> = seq [1; 2; 3]
*)


// ещё один пример сравнения последовательностей и списков
let list12 = [for a in 5..-1..0 -> 10 / a] // ошибка деления на ноль
let seq5 = seq {for a in 5..-1..0 -> 10 / a} // ошибок нет
seq5 |> Seq.take 5 |> Seq.iter (printfn "%i") // ошибок нет
seq5 |> Seq.take 6 |> Seq.iter (printfn "%i") // 2, 2, 3, 5, 10, ошибка деления на ноль

// бесконечные последовательности
// чётные числа
let seq6 =
  let rec seq6' x = seq { yield x; yield! seq6' (x + 2) } // рекурсивное создание последовательности
  seq6' 0

Seq.take 10 seq6 // val it : seq<int> = seq [0; 2; 4; 6; ...]

// пример из самостоятельной работы
let seq7 = 
    let rec seq7' x = seq { yield! [0; x]; yield! seq7' (x + 1) }
    seq7' 1;;
Seq.take 10 seq7 // val it : seq<int> = seq [0; 1; 0; 2; ...]

let rec ones' = 1 :: ones' // бесконечный список из единиц как в Haskell'е не сделать..
let rec ones = seq {yield 1; yield! ones} // но его можно реализовать так
Seq.take 100 ones // val it : seq<int> = seq [1; 1; 1; 1; ...]
let ones'' = Seq.initInfinite (fun _ -> 1) // или так, с использованием функции initInfinite

(* 
Генерация последовательностей с использованием функции Seq.unfold
тип этой функции ('a -> ('b * 'a) option) -> 'a -> seq<'b>
первый аргумент - функция для генерации нового элемента последовательности из состояния, она должна возвращать известный нам тип option, заключающий в себе кортеж из этого самого нового элемента и нового состояния
второй аргумент - начальное состояние
*)
// создаём список из чётных чисел меньше ста
let seq8 = Seq.unfold (fun state -> if state < 100 then Some(state, state+2) else None) 2
seq8 |> Seq.iter (printf "%i ")

// генерация бесконечной последовательности чисел Фиббоначе
let fibb = Seq.unfold (fun state -> Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)
Seq.take 20 fibb

