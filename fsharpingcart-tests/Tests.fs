module Tests

open Xunit

type ItemType = ItemType of string

type PercentDiscount =
    { PercentOff : double }

type FixedAmountDiscount =
    { Amount : decimal }

type Discount =
    | Percent of PercentDiscount
    | FixedAmount of FixedAmountDiscount

type ItemCode = ItemCode of string

type Item =
    { Code : ItemCode
      Cost : decimal
      Discounts : Discount list
      Type : ItemType }

type CouponCode = CouponCode of string

type Coupon =
    { Code : CouponCode
      Discount : PercentDiscount
      ItemType : ItemType option}

let applyDiscount (cost: decimal) (discount: Discount) = 
    let discountedAmount =
        match discount with
        | FixedAmount fixedAmount -> fixedAmount.Amount
        | Percent percent -> decimal (percent.PercentOff) * cost
    cost - discountedAmount

let getCouponDiscountForItemType (itemType : ItemType) (coupon : Coupon) : PercentDiscount option = 
    match coupon.ItemType with
    | None -> Some(coupon.Discount)
    | Some couponItemType -> 
        if couponItemType = itemType then
            Some(coupon.Discount)
        else
            None

let computeDiscountedCost (item : Item) (coupon : Coupon option) =
    match item.Discounts with
    | [] -> item.Cost
    | _::_ ->
        let allDiscounts =
            match coupon with
            | Some coupon -> 
                match getCouponDiscountForItemType item.Type coupon with
                | Some discount -> Discount.Percent discount::item.Discounts
                | None -> item.Discounts
            | None -> item.Discounts
        
        allDiscounts
            |> List.map (fun discount -> applyDiscount item.Cost discount)
            |> List.min

type ShoppingItem =
    { ItemCode : ItemCode
      Quantity : double }

type Shop =
    { Items : Item list
      Coupons : Coupon list}

let compute (shop: Shop) (shoppingItems : ShoppingItem list) (couponCode : CouponCode option) =
    let coupon =
        match couponCode with 
        | Some couponCode ->
            shop.Coupons
                |> List.tryFind (fun coupon -> coupon.Code = couponCode)
        | None -> None

    shoppingItems
        |> List.map (fun shoppingItem ->
            let matchingItem = shop.Items |> List.find (fun item -> item.Code = shoppingItem.ItemCode)
            decimal (shoppingItem.Quantity) * computeDiscountedCost matchingItem coupon)
        |> List.sum

[<Fact>]
let ``User enters several Shopping Items and program returns Total Cost``() =
    let generalItemType = ItemType "general"
    
    let shop =
        { Items =
              [ { Code = ItemCode "banana"
                  Discounts = []
                  Type = generalItemType
                  Cost = 20.0m }
                { Code = ItemCode "apple"
                  Discounts = []
                  Type = generalItemType
                  Cost = 25.0m }
                { Code = ItemCode "lettuce"
                  Discounts = []
                  Type = generalItemType
                  Cost = 5.0m }
                { Code = ItemCode "cabbage"
                  Discounts = []
                  Type = generalItemType
                  Cost = 6.0m }
                { Code = ItemCode "peach"
                  Discounts = []
                  Type = generalItemType
                  Cost = 12.0m } ] 
          Coupons = [] }

    let shoppingItems =
        [ { ItemCode = ItemCode "banana"
            Quantity = 2.0 }
          { ItemCode = ItemCode "apple"
            Quantity = 1.0 }
          { ItemCode = ItemCode "cabbage"
            Quantity = 2.0 } ]

    let totalCost = compute shop shoppingItems None
    Assert.Equal(77.0m, totalCost)

[<Fact>]
let ``Items can be on special``() =
    let generalItemType = ItemType "general"

    let shop =
        { Items =
               [{ Code = ItemCode "banana"
                  Discounts = [Discount.FixedAmount { Amount = 10.0m }]
                  Type = generalItemType
                  Cost = 20.0m }
                { Code = ItemCode "apple"
                  Discounts = [Discount.Percent { PercentOff = 0.20 }]
                  Type = generalItemType
                  Cost = 25.0m }
                { Code = ItemCode "lettuce"
                  Discounts = []
                  Type = generalItemType
                  Cost = 5.0m }
                { Code = ItemCode "cabbage"
                  Discounts = 
                    [ Discount.Percent { PercentOff = 0.20 };
                      Discount.FixedAmount { Amount = 3.0m }]
                  Type = generalItemType
                  Cost = 6.0m }
                { Code = ItemCode "peach"
                  Discounts = []
                  Type = generalItemType
                  Cost = 12.0m } ] 
          Coupons = [] }

    let shoppingItems =
        [ { ItemCode = ItemCode "banana"
            Quantity = 2.0 }
          { ItemCode = ItemCode "apple"
            Quantity = 3.0 }
          { ItemCode = ItemCode "cabbage"
            Quantity = 2.0 } ]

    let totalCost = compute shop shoppingItems None
    Assert.Equal(86.0m, totalCost)

[<Fact>]
let ``User can use a coupon code``() =
    let generalItemType = ItemType "general"

    let shop =
        { Items =
               [{ Code = ItemCode "banana"
                  Discounts = [Discount.FixedAmount { Amount = 10.0m }]
                  Type = generalItemType
                  Cost = 20.0m }
                { Code = ItemCode "apple"
                  Discounts = [Discount.Percent { PercentOff = 0.20 }]
                  Type = generalItemType
                  Cost = 25.0m }
                { Code = ItemCode "lettuce"
                  Discounts = []
                  Type = generalItemType
                  Cost = 5.0m }
                { Code = ItemCode "cabbage"
                  Discounts = 
                    [ Discount.Percent { PercentOff = 0.20 };
                      Discount.FixedAmount { Amount = 3.0m }]
                  Type = generalItemType
                  Cost = 6.0m }
                { Code = ItemCode "peach"
                  Discounts = []
                  Type = generalItemType
                  Cost = 12.0m } ] 
          Coupons = 
            [{ Code = CouponCode "80OFF"; 
               Discount = { PercentOff = 0.80 }; 
               ItemType = None }] }

    let shoppingItems =
        [ { ItemCode = ItemCode "banana"
            Quantity = 2.0 }
          { ItemCode = ItemCode "apple"
            Quantity = 3.0 }
          { ItemCode = ItemCode "cabbage"
            Quantity = 2.0 } ]

    let coupon = CouponCode "80OFF"

    let totalCost = compute shop shoppingItems (Some coupon)
    Assert.Equal(25.4m, totalCost)

[<Fact>]
let ``User can use a coupon for item types``() =
    let fruitsItemType = ItemType "fruit"
    let vegetableItemType = ItemType "veg"

    let shop =
        { Items =
               [{ Code = ItemCode "banana"
                  Discounts = [Discount.FixedAmount { Amount = 10.0m }]
                  Type = fruitsItemType
                  Cost = 20.0m }
                { Code = ItemCode "apple"
                  Discounts = [Discount.Percent { PercentOff = 0.20 }]
                  Type = fruitsItemType
                  Cost = 25.0m }
                { Code = ItemCode "lettuce"
                  Discounts = []
                  Type = vegetableItemType
                  Cost = 5.0m }
                { Code = ItemCode "cabbage"
                  Discounts = []
                  Type = vegetableItemType
                  Cost = 6.0m }
                { Code = ItemCode "peach"
                  Discounts = []
                  Type = fruitsItemType
                  Cost = 12.0m } ] 
          Coupons = 
            [{ Code = CouponCode "80OFF_FRUITS"; 
               Discount = { PercentOff = 0.80 }; 
               ItemType = Some(fruitsItemType) }] }

    let shoppingItems =
        [ { ItemCode = ItemCode "banana"
            Quantity = 2.0 }
          { ItemCode = ItemCode "apple"
            Quantity = 3.0 }
          { ItemCode = ItemCode "cabbage"
            Quantity = 2.0 } ]

    let coupon = CouponCode "80OFF_FRUITS"

    let totalCost = compute shop shoppingItems (Some coupon)
    Assert.Equal(35m, totalCost)