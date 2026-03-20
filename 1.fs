open System

type Tree = 

    | Node of int * Tree * Tree 
    | Empty

let rec recPrint tree = 
    match tree with 

    | Node (data, left, right) -> 
        recPrint left
        printfn "Node %d" data
        recPrint right 
    | Empty -> ()

let rec Map f tree =
    match tree with

    | Node (data, left, right) -> 
        Node (f data, Map f left, Map f right)
    | Empty -> Empty

let rnd = Random()

let binTree =
    Node(rnd.Next(0, 10),
        Node(rnd.Next(0, 10), Empty, Empty),
        Node(rnd.Next(0, 10),
            Node(rnd.Next(0, 10), Empty, Empty),
            Node(rnd.Next(0, 10), Empty, Empty)
        )
    )

[<EntryPoint>]
let main argv =
    printfn "--- Исходное дерево  ---"
    recPrint binTree

    let fixedTree = Map (fun x -> if x = 9 then 9 else x + 1) binTree

    printfn "\n--- После изменения  ---"
    recPrint fixedTree
    
    0
