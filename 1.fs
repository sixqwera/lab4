open System

type Tree = 

    | Node of int * Tree * Tree 
    | Empty

let rec recPrint indent tree = 
    match tree with 

    | Node (data, left, right) -> 
        recPrint (indent + "    ") right
        printfn "%s%d" indent data
        recPrint (indent + "    ") left
    | Empty -> ()

let rec insert value tree =
    match tree with

    | Empty -> Node(value, Empty, Empty)
    | Node(x, left, right) ->
        if value < x then Node(x, insert value left, right)
        else Node(x, left, insert value right)

let rec treeMap f tree =
    match tree with

    | Node (data, left, right) -> 
        Node (f data, treeMap f left, treeMap f right)
    | Empty -> Empty

let rec getValidCount() =
    printf "Введите количество элементов дерева: "
    let input = Console.ReadLine()
    match Int32.TryParse(input) with

    | true, v when v > 0 -> v  
    | _ -> 
        printfn "Ошибка: нужно ввести целое положительное число"
        getValidCount() 

[<EntryPoint>]
let main argv =
    let rnd = Random()
    let count = getValidCount()

    let sourceList = List.init count (fun _ -> rnd.Next(0, 10))
    let binTree =
        List.fold (fun acc x -> insert x acc) Empty sourceList

    printfn "\n--- Исходное дерево (повернуто набок) ---"
    recPrint "" binTree

    let updatedTree = 
        treeMap (fun x -> if x = 9 then 9 else x + 1) binTree

    printfn "\n--- После обработки (+1 к значениям) ---"
    recPrint "" updatedTree
    
    0
