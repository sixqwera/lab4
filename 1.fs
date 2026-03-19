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

let rec treeMap f tree =
    match tree with

    | Node (data, left, right) -> 
        Node (f data, treeMap f left, treeMap f right)
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

    let incrementedTree = treeMap (fun x -> if x = 9 then 9 else x + 1) binTree

    printfn "\n--- После изменения  ---"
    recPrint incrementedTree
    
    0
