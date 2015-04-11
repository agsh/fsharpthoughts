open System

let explode (s:string) =
  [for c in s -> c]

type Token =
  | OpenBrace | CloseBrace
  | OpenBracket | CloseBracket
  | Colon | Comma
  | String of string
  | Number of int
  | Boolean of bool
  | Null

let tokenize source =
  let rec parseString acc = function
    | '\\' :: '"' :: t -> parseString (acc + "\"") t
    | '"' :: t -> acc, t
    | c :: t -> parseString (acc + c.ToString()) t
    | _ -> failwith "Malformed string."
 
  let rec token acc = function
    | (x :: _) as t when List.exists ((=)x) [')'; ':'; ','; ']'] -> acc, t
    | w :: t when Char.IsWhiteSpace(w) -> acc, t // whitespace terminates
    | [] -> acc, [] // end of list terminates
    | c :: t -> token (acc + (c.ToString())) t // otherwise accumulate chars

  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t   // skip whitespace
    | '{' :: t -> tokenize' (OpenBrace :: acc) t
    | '}' :: t -> tokenize' (CloseBrace :: acc) t
    | '[' :: t -> tokenize' (OpenBracket :: acc) t
    | ']' :: t -> tokenize' (CloseBracket :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '"' :: t -> // start of string
      let s, t' = parseString "" t
      tokenize' (String s :: acc) t'
    | d :: t when Char.IsDigit(d) -> // start of negative number
      let n, t' = token (d.ToString()) t
      tokenize' (Number (Convert.ToInt32 n)  :: acc) t'
    | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
    | [] -> List.rev acc // end of list terminates
    | _ -> failwith "Tokinzation error"
  tokenize' [] source

type JSON =
  | Object of (string * JSON) list
  | Array of JSON list
  | Number of int
  | String of string
  | Boolean of bool
  | Null

let rec parse json =
  let rec parse' json =
    let rec parseObject list = function
      | CloseBrace :: t -> (Object (List.rev list)), t
      | Comma :: t -> parseObject list t
      | Token.String s :: Colon :: t ->
        let a, t = parse' t
        parseObject ((s, a) :: list) t
      | _ -> failwith "Incorrect object"
    let rec parseArray list = function
      | CloseBracket :: t -> (Array (List.rev list)), t
      | Comma :: t -> parseArray list t
      | ob -> 
        let a, t = parse' ob
        parseArray (a :: list) t  
    match json with
      | OpenBrace :: t -> parseObject [] t
      | OpenBracket :: t -> parseArray [] t
      | Token.Null :: t -> JSON.Null, t
      | Token.String s :: t -> JSON.String s, t
      | Token.Number s :: t -> JSON.Number s, t
      | Token.Boolean s :: t -> JSON.Boolean s, t
      | _ -> failwith "Incorrect identification"
  match parse' json with
    | res, [] -> res
    | _ -> failwith "Wrong JSON structure"


let s = """
{
  "a": 1
  "b": {
    "c": [1,2,3]
  }
}
"""

open System.Windows.Forms
open System.Drawing

let tree = s |> explode |> tokenize |> parse

let rec treeLoop (node:JSON) : TreeNode =
  match node with
    | JSON.Number i -> new TreeNode(i.ToString())
    | JSON.Array list ->
      let root = new TreeNode("[]")
      List.map (fun v -> 
          root.Nodes.Add(treeLoop v)
        ) list
      root
    | JSON.Object list ->
      let root = new TreeNode("{}")
      List.map (fun (k, v) -> 
          let r = root.Nodes.Add(k.ToString())
          r.Nodes.Add(treeLoop v)
        ) list
      root

let form = new Form(Height=400, Width=250, Text="JSON show", StartPosition=FormStartPosition.CenterScreen, MinimizeBox=false, ShowIcon=false)

let treeView = new TreeView(Dock = DockStyle.Fill)
treeView.Nodes.Add(treeLoop tree) |> ignore
treeView.ExpandAll()
form.Controls.Add(treeView)

form.Show()
Application.Run()
