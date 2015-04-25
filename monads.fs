// Computation Expressions

let oReturn (x: 'a) : 'a option = Some(x)

let oReturnFrom (x: 'a option) : 'a option = x

let oBind (mx: 'a option) (f: 'a -> 'b option) : 'b option = 
    match mx with
    | Some(x) -> f x
    | None -> None
                                                             
let (>>=) = oBind

type OptionMonad() =
    member x.Bind(p, f) = oBind p f
    member x.Return(y) = oReturn y
    member x.ReturnFrom(y) = oReturnFrom y

let option = new OptionMonad()

let isEven x = if x % 2 = 0 then Some x else None 
let minus2 x = Some(x - 2)
let div10 x = if x = 0 then None else Some(10 / x)
Some(4) >>= isEven >>= minus2 >>= div10
Some(5) >>= isEven >>= minus2 >>= div10
Some(2) >>= isEven >>= minus2 >>= div10

let a = option {
    let! a = Some(2)
    let! b = isEven a
    let! c = minus2 b
    let! d = div10 c
    return d
}
let a = option {
    let! a = Some(5)
    let! b = isEven a
    let c = b - 2
    let! d = div10 c
    return d
}

let lReturn (x: 'a) : 'a list = [x]

let lReturnFrom (x: 'a list) : 'a list = x

let lBind (mx: 'a list) (f: 'a -> 'b list) : 'b list =
    mx |> List.map f |> List.concat

let (>>=) = lBind

type ListMonad() =
    member x.Bind(p, f) = lBind p f
    member x.Return(y) = lReturn y
    member x.ReturnFrom(y) = lReturnFrom y

let list = new ListMonad()

[1;2;3] >>= (fun x -> [x; x+1; x+2])

let a = list {
    let! a = [1;2;3]
    let c = 13
    let! b = [4;5;6]
    printf "%A-%A-%A\n" a b c
    return a
    // return! [a;a+1;a+2]
} 
