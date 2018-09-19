module Tests

open System
open Xunit

type Item = {
    Code: string;
    Price: float
}

type CartItem = {
    ItemCode: string;
    Quantity: float;
}

let getByItemCode (items : Item seq) (itemCode : string) : option<Item> = 
    items |> Seq.tryFind (fun item -> item.Code = itemCode)

let compute (purchasedItems : CartItem seq) (items : Item seq) = 
    purchasedItems 
        |> Seq.map 
            (fun purchasedItem -> 
                match (getByItemCode items purchasedItem.ItemCode) with
                    | Some i -> purchasedItem.Quantity * i.Price
                    | None -> 0.0)
        |> Seq.sum

[<Fact>]
let ``Zero items`` () =
    let banana = { Code = "Banana"; Price = 12.00 };
    let potato = { Code = "Potato"; Price = 18.00 };
    let items = [
        banana;
        potato
    ]

    let purchasedItems = [
        { ItemCode = "Banana"; Quantity = 5.0 };
        { ItemCode = "Potato"; Quantity = 2.0 }
    ]

    let totalCost = compute purchasedItems items
    Assert.Equal(96.0, totalCost)