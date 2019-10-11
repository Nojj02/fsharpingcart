module Tests

open System
open Xunit

type ItemType = string

type PercentDiscount =
    { PercentOff : double }

type FixedAmountDiscount =
    { Amount : decimal }

type Discount =
    | Percent of PercentDiscount
    | FixedAmount of FixedAmountDiscount

type ItemDiscount =
    | OnSpecial of Discount
    | NoDiscount

type ItemCode = string

type Item =
    { Code : ItemCode
      Cost : decimal
      DiscountState : ItemDiscount
      Type : ItemType }

type ShoppingItem =
    { ItemCode : ItemCode
      Quantity : double }
      3
type CouponCode = string

type Coupon =
    { Code : CouponCode
      Discount : Discount }

type Shop =
    { Items : Item list }

let compute (shop : Shop) (shoppingItems : ShoppingItem list) =
    shoppingItems
    |> List.map (fun shoppingItem ->
        let matchingItem = shop.Items |> List.find (fun item -> item.Code = shoppingItem.ItemCode)
        decimal (shoppingItem.Quantity) * matchingItem.Cost)
    |> List.sum

[<Fact>]
let ``User enters several Shopping Items and program returns Total Cost``() =
    let generalItemType = "general"

    let shop =
        { Items =
              [ { Code = "banana"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 20.0m }
                { Code = "apple"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 25.0m }
                { Code = "mango"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 30.0m }
                { Code = "potato"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 15.0m }
                { Code = "lettuce"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 5.0m }
                { Code = "cabbage"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 6.0m }
                { Code = "peach"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 12.0m }
                { Code = "corn"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 17.0m }
                { Code = "spinach"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 22.0m }
                { Code = "onion"
                  DiscountState = ItemDiscount.NoDiscount
                  Type = generalItemType
                  Cost = 8.0m } ] }

    let shoppingItems =
        [ { ItemCode = "banana"
            Quantity = 2.0 }
          { ItemCode = "apple"
            Quantity = 1.0 }
          { ItemCode = "cabbage"
            Quantity = 2.0 } ]

    let totalCost = compute shop shoppingItems
    Assert.Equal(77.0m, totalCost)