

option { for i in [1;2;3] do i }

option { 1 }

option {
    let x = 1           
    let! x = (Some 1)   
    return 1           
    return! (Some 1)   
    yield 1            
    yield! (Some 1)    
}

option {
    let! x = option { return 1 }
    let! a = option { return 2 }
    return a     
}

type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        oBind m f // Option.bind f m
    member y.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x
    member y.ReturnFrom(m) = 
        printfn "Returning an option (%A)" m
        m

let trace = new Trace()

trace { 
    return 1
} |> printfn "Result 1: %A" 

trace { 
    let! x = Some 1
    let! y = Some 2
    return x + y
} |> printfn "Result 2: %A" 

// TODO use trace.Bind and trace.Return

trace.Bind (Some 1, fun x ->
    trace.Bind (Some 2, fun y ->
        trace.Return (x+y)
    )
) |> printfn "Result 2: %A" 

trace { 
    let! x = None
    let! y = Some 1
    return x + y
} |> printfn "Result 3: %A" 

trace { 
    do! Some (printfn "expression that returns unit")
    do printfn "another expression that returns unit"
    let! _ = Some(printfn "do is another let binding")
    let! y = Some (printfn "and another expression that returns unit")
    let! x = Some (1)
    return x
} |> printfn "Result 4: %A" 

trace { 
} |> printfn "Result for empty: %A" 

trace { 
    printfn "expression that returns unit" // if false then return 1
} |> printfn "Result for simple expression: %A" 

type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        oBind m f // Option.bind f m
    member y.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x
    member y.ReturnFrom(m) = 
        printfn "Returning an option (%A)" m
        m
    member y.Zero() = 
        printfn "Zero"
        None
       
let trace = new Trace()

trace { 
    printfn "unit" 
} |> printfn "Result for unit: %A" 

let s = seq { printfn "zero" } 
let a = async { printfn "zero" }

trace { 
    yield 1
} |> printfn "Result for yield: %A" 

type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        oBind m f // Option.bind f m
    member y.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x
    member y.ReturnFrom(m) = 
        printfn "Returning an option (%A)" m
        m
    member y.Zero() = 
        printfn "Zero"
        None
    member y.Yield(x) = 
        printfn "Yield an unwrapped %A as an option" x
        y.Return(x)
    member y.YieldFrom(m) = 
        printfn "Yield an option (%A) directly" m
        m
    member y.For(m,f) =
        printfn "For %A" m
        y.Bind(m,f)
       
let trace = new Trace()

trace { 
    yield 1
} |> printfn "Result for yield: %A"

trace { 
    yield! Some 1
} |> printfn "Result for yield: %A"

let s = seq {yield 1} 
let s = seq {return 1}

let a = async {return 1}
let a = async {yield 1}

trace {
    for i in Some(1) do
        for j in Some(2) do // None
            return i + j
}

type ListMonad() =
    member y.Bind(m, f) = m |> List.map f |> List.concat // List.collect f
    member y.Zero() = 
        printfn "Zero"
        []
    member y.Return(x) = 
        printfn "Return an unwrapped %A as a list" x
        [x]
    member y.Yield(x) = 
        printfn "Yield an unwrapped %A as a list" x
        [x]        
    member y.For(m,f) =
        printfn "For %A" m
        y.Bind(m,f)
                        
let list = new ListMonad()

list { 
    let! x = [1..3]
    let! y = [10;20;30]
    return x + y
} |> printfn "Result: %A"

let a = list { 
    for x in [1..3] do
        for y in [10;20;30] do
            yield x + y
} |> printfn "Result: %A"

trace { 
    yield 1
    yield 2
} |> printfn "Result for yield then yield: %A"

trace { 
    let! x = Some 1
    return x
    return 2
} |> printfn "Result for return then return: %A"

oBind (Some 1) (fun x -> 
    oBind (Some 2) (fun y -> 
        oBind (Some (x + y)) (fun z -> 
            oReturn(z)  // or yield 
)))

let oYield = oReturn

(*
cexpr1
cexpr2 

b.Combine(cexpr1, b.Delay(fun () -> cexpr2))
*)

oBind (Some 1) (fun x -> 
    oBind (Some 2) (fun y -> 
        oBind (Some (x + y)) (fun z -> 
            oYield(z)  // or return 
)))
oBind (Some 3) (fun x -> 
    oBind (Some 4) (fun y -> 
        oBind (Some (x + y)) (fun z -> 
            oYield(z)  // or return 
)))

type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        oBind m f // Option.bind f m
    member y.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x
    member y.ReturnFrom(m) = 
        printfn "Returning an option (%A)" m
        m
    member y.Zero() = 
        printfn "Zero"
        None
    member y.Yield(x) = 
        printfn "Yield an unwrapped %A as an option" x
        Some x
    member y.YieldFrom(m) = 
        printfn "Yield an option (%A) directly" m
        m
    member y.For(m,f) =
        printfn "For %A" m
        y.Bind(m,f)
    member y.Combine(a,b) = // a:int option * b:int option -> int option
        match a,b with
        | Some a', Some b' ->
            printfn "combining %A and %A" a' b' 
            Some (a' + b')
        | Some a', None ->
            printfn "combining %A with None" a' 
            Some a'
        | None, Some b' ->
            printfn "combining None with %A" b' 
            Some b'
        | None, None ->
            printfn "combining None with None"
            None
    member y.Delay(f) = 
        printfn "Delay"
        f()

let trace = new Trace()

trace { 
    let! a = Some 1
    yield a
    let! b = Some 2
    yield b
} |> printfn "Result for yield then yield: %A" 

trace { 
    yield 1
    let! x = None
    yield 2
} |> printfn "Result for yield then None: %A" 

trace { 
    yield 1
    return 2
    yield 3
} |> printfn "Result for yield, return, yield: %A" 

type ListMonad() =
    member y.Bind(m, f) = m |> List.collect f
    member y.Zero() = 
        printfn "Zero"
        []
    member y.Yield(x) = 
        printfn "Yield an unwrapped %A as a list" x
        [x]
    member y.YieldFrom(m) = 
        printfn "Yield a list (%A) directly" m
        m
    member y.For(m,f) =
        printfn "For %A" m
        y.Bind(m,f)        
    member y.Combine (a,b) = 
        printfn "combining %A and %A" a b 
        List.concat [a;b]
    member y.Delay(f) = 
        printfn "Delay"
        f()
               
let list = new ListMonad()

list { 
    yield 1
    yield 2
} |> printfn "Result for yield then yield: %A" 

list { 
    yield 1
    yield! [2;3]
} |> printfn "Result for yield then yield! : %A" 

list { 
    for i in [1; 2] do
        yield i
        for j in [3; 4] do
            yield! [i + j; 0]
} |> printfn "Result for for..in..do : %A" 

list { 
    yield 1
    yield 2
    yield 3
    yield 4
} |> printfn "Result for yield x 4: %A" 

type Trace2() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        oBind m f // Option.bind f m
    member y.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x
    member y.ReturnFrom(m) = 
        printfn "Returning an option (%A)" m
        m
    member y.Zero() = 
        printfn "Zero"
        None
    member y.Yield(x) = 
        printfn "Yield an unwrapped %A as an option" x
        Some x
    member y.YieldFrom(m) = 
        printfn "Yield an option (%A) directly" m
        m
    member y.For(m,f) =
        printfn "For %A" m
        y.Bind(m,f)
    member y.Combine(a,b) =
        printfn "Combining" 
        match a with
        | None -> b
        | Some x -> a
    member y.Delay(f) = 
        printfn "Delay"
        f()

let trace2 = new Trace2()

let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList

trace2 { 
    return! (map1.TryFind "2") // map2
    return! (map2.TryFind "A") // Z
} |> printfn "Result for map lookup: %A" 


let combine a b = 
    match a with
    | Some _ -> a  
    | None -> b   

let ( <++ ) = combine

let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList

let result = 
    (map1.TryFind "A") 
    <++ (map1.TryFind "B")
    <++ (map2.TryFind "A")
    <++ (map2.TryFind "B")
    |> printfn "Result of adding options is: %A"

type OrElse() =
    member x.ReturnFrom(y) = y
    member x.Combine (a,b) = 
        match a with
        | Some _ -> a 
        | None -> b
    member x.Delay(f) = f()

let orElse = new OrElse()

let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList
let map3 = [ ("H","Haskell"); ("F","F Sharp") ] |> Map.ofList

let multiLookup key = orElse {
    return! map1.TryFind key
    return! map2.TryFind key
    return! map3.TryFind key
}

multiLookup "A" |> printfn "Result for A is %A" 
multiLookup "F" |> printfn "Result for F is %A" 
multiLookup "X" |> printfn "Result for X is %A" 


type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m
    member y.Return(x) = 
        printfn "Return an unwrapped %A as an option" x
        Some x
    member y.Zero() = 
        printfn "Zero"
        None
    member y.Combine (a,b) = 
        printfn "Returning early with %A. Ignoring second part: %A" a b 
        a
    member y.Delay(f) = // (unit -> 'a) -> 'a
        printfn "Delay"
        f()
              
let trace = new Trace()

trace { 
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
} |> printfn "Result for Part1 without Part2: %A" 

type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m
    member y.Return(x) = 
        printfn "Return an unwrapped %A as an option" x
        Some x
    member y.Zero() = 
        printfn "Zero"
        None
    member y.Combine (a,b) = 
        printfn "Returning early with %A. Ignoring second part: %A" a b 
        a
    member y.Delay(funcToDelay) = // (unit -> 'a) -> (unit -> 'a)
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult  
        printfn "%A - Delaying using %A" 
        delayed
              
let trace = new Trace()

trace { 
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
} |> printfn "Result for Part1 without Part2: %A" 
   
let f = trace { 
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
} 
f() |> printfn "Result for Part1 without Part2: %A" 

type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m
    member y.Return(x) = 
        printfn "Return an unwrapped %A as an option" x
        Some x
    member y.Zero() = 
        printfn "Zero"
        None
    member y.Combine (a,b) = 
        printfn "Returning early with %A. Ignoring second part: %A" a b 
        a
    member y.Delay(funcToDelay) = 
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult  
        printfn "%A - Delaying using %A" funcToDelay delayed
        delayed
    member y.Run(funcToRun) = 
        printfn "%A - Run Start." funcToRun
        let runResult = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun runResult
        runResult

let trace = new Trace()

trace { 
    printfn "Part 1: about to return 1"
    return 1
    printfn "Part 2: after return has happened"
} |> printfn "Result for Part1 without Part2: %A"


type Trace() =
    member y.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m
    member y.Return(x) = 
        printfn "Return an unwrapped %A as an option" x
        Some x
    member y.Zero() = 
        printfn "Zero"
        None
    member y.Combine (m,f) = // m:int option * f:(unit -> int option) -> int option
        printfn "Combine. Starting second param %A" f
        let z = f()
        printfn "Combine. Finished second param %A. Result is %A" f y
        match m,z with
        | Some a, Some b ->
            printfn "combining %A and %A" a b 
            Some (a + b)
        | Some a, None ->
            printfn "combining %A with None" a 
            Some a
        | None, Some b ->
            printfn "combining None with %A" b 
            Some b
        | None, None ->
            printfn "combining None with None"
            None
    member y.Delay(funcToDelay) = 
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult  
        printfn "%A - Delaying using %A" funcToDelay delayed
        delayed
    member y.Run(funcToRun) = 
        printfn "%A - Run Start." funcToRun
        let runResult = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun runResult
        runResult

let trace = new Trace()

trace { 
    return 1
    return 2
} |> printfn "Result for return then return: %A" 



open System

let strToInt str = ???
Int32.TryParse("19")

let stringAddWorkflow x y z =
    addWorkflow {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
    }

printfn "%A" (stringAddWorkflow "12" "3" "2")  
printfn "%A" (stringAddWorkflow "12" "xyz" "2") 

let strAdd str i = ???
let (>>=) m f = ???

printfn "%A" (strToInt "1" >>= strAdd "2" >>= strAdd "3") 
printfn "%A" (strToInt "1" >>= strAdd "z" >>= strAdd "3")







let strToInt str = 
    match Int32.TryParse(str) with
    | false,_ -> None
    | true,x  -> Some x

type AddWorkflow() =
    member this.Bind(m,f) = Option.bind f m
    member this.Return(x) = Some x

let addWorkflow = new AddWorkflow()

let strAdd str i =
    strToInt str |> Option.map ((+) i)

let (>>=) m f = Option.bind f m
