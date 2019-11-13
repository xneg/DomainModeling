module internal DomainModeling.Domain.PlaceOrderWorkflow.Steps

open DomainModeling.Domain.Api
open DomainModeling.Domain.Primitives
open DomainModeling.Domain.Utils
open DomainModeling.Domain.Result

// Validation

type CheckedAddress = CheckedAddress of UnvalidatedAddress

type AddressValidationError =
    | InvalidFormat of string
    | AddressNotFound of string

type ValidateOrder =
    CheckProductCodeExists
        -> CheckAddressExists
        -> UnvalidatedOrder
        -> Result<ValidatedOrder, ValidationError> // AsyncResult<ValidatedOrder, ValidationError list>
and
    CheckProductCodeExists = ProductCode -> bool
and
    CheckAddressExists = UnvalidatedAddress -> Result<CheckedAddress, AddressValidationError> //AsyncResult<CheckedAddress, AddressValidationError>

// Pricing
        
type PriceOrder =
    GetProductPrice
        -> ValidatedOrder
        -> Result<PricedOrder, PricingError>
and
    GetProductPrice = ProductCode -> Price

// Acknowledgment
        
type HtmlString = HtmlString of string

type OrderAcknowledgment = {
    EmailAddress: EmailAddress
    Letter: HtmlString
}
type SentResult = Sent | NotSent

type AcknowledgeOrder =
    CreateOrderAcknowledgmentLetter
        -> SendOrderAcknowledgment
        -> PricedOrder
        -> OrderAcknowledgmentSent option
and
    CreateOrderAcknowledgmentLetter = PricedOrder -> HtmlString
and
    SendOrderAcknowledgment = OrderAcknowledgment -> SentResult
        
// Create events

type CreateEvents = PricedOrder -> OrderAcknowledgmentSent option -> PlaceOrderEvent list

// implementation

// Validation
let toCustomerInfo (customer:UnvalidatedCustomerInfo) =
    result {
        let! firstName = customer.FirstName |> String50.create "FirstName" |> Result.mapError ValidationError
        let! lastName = customer.LastName |> String50.create "LastName" |> Result.mapError ValidationError
        let! emailAddress = customer.EmailAddress |> EmailAddress.create "EmailAddress" |> Result.mapError ValidationError
        
        let customerInfo = {
            Name = {FirstName = firstName; LastName = lastName}
            EmailAddress = emailAddress
        }
        return customerInfo
    }

let toAddress (CheckedAddress unvalidatedAddress) =
    result {
        let! addressLine1 = 
            unvalidatedAddress.AddressLine1 
            |> String50.create "AddressLine1" 
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! addressLine2 = 
            unvalidatedAddress.AddressLine2 
            |> String50.createOption "AddressLine2"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! addressLine3 = 
            unvalidatedAddress.AddressLine3 
            |> String50.createOption "AddressLine3" 
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! addressLine4 = 
            unvalidatedAddress.AddressLine4 
            |> String50.createOption "AddressLine4"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! city = 
            unvalidatedAddress.City
            |> String50.create "City"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! zipCode = 
            unvalidatedAddress.ZipCode
            |> ZipCode.create "ZipCode"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let address : Address = {
            AddressLine1 = addressLine1
            AddressLine2 = addressLine2
            AddressLine3 = addressLine3
            AddressLine4 = addressLine4
            City = city
            ZipCode = zipCode
            }
        return address
    }

//let toAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress =
//    result {
//        let! checkedAddress = checkAddressExists unvalidatedAddress
//        let addressLine1 = checkedAddress.AddressLine1 |> String50.create "AddressLine1"
//        let addressLine2 = checkedAddress.AddressLine2 |> String50.create "AddressLine2"
//        let addressLine3 = checkedAddress.AddressLine3 |> String50.create "AddressLine3"
//        let addressLine4 = checkedAddress.AddressLine4 |> String50.create "AddressLine4"
//
//        let city = checkedAddress.City |> String50.create "City"
//        let zipCode = checkedAddress.ZipCode |> ZipCode.create "ZipCode"
//        let address: Address = {
//            AddressLine1 = addressLine1
//            AddressLine2 = Some(addressLine2)
//            AddressLine3 = Some(addressLine3)
//            AddressLine4 = Some(addressLine4)
//            City = city
//            ZipCode = zipCode
//        }
//        return address
//    }
//    failwith "Undefined"

//    let (CheckedAddress checkedAddress) = checkedAddress
//    
//    let addressLine1 = checkedAddress.AddressLine1 |> String50.create "AddressLine1"
//    let addressLine2 = checkedAddress.AddressLine2 |> String50.create "AddressLine2"
//    let addressLine3 = checkedAddress.AddressLine3 |> String50.create "AddressLine3"
//    let addressLine4 = checkedAddress.AddressLine4 |> String50.create "AddressLine4"
//    let city = checkedAddress.City |> String50.create "City"
//    let zipCode = checkedAddress.ZipCode |> ZipCode.create "ZipCode"
//    
//    let address: Address = {
//        AddressLine1 = addressLine1
//        AddressLine2 = Some(addressLine2)
//        AddressLine3 = Some(addressLine3)
//        AddressLine4 = Some(addressLine4)
//        City = city
//        ZipCode = zipCode
//    }
//    address

let toProductCode (checkProductCodeExists: CheckProductCodeExists) productCode =
    if checkProductCodeExists productCode then
        Ok productCode
    else
        let errorMsg = sprintf "Invalid: %A" productCode
        Error (ValidationError errorMsg)

let toOrderQuantity productCode quantity =
    match productCode with
    | Widget _ ->
        quantity |> int |> UnitQuantity.create "Quantity" |> OrderQuantity.Unit
    | Gizmo _ ->
        quantity |> decimal |> KilogramQuantity.create "Quantity" |> OrderQuantity.Kilos

let toValidatedOrderLine checkProductCodeExists (unvalidatedOrderLine: UnvalidatedOrderLine) =
    result {
        let! orderLineId = unvalidatedOrderLine.OrderLineId |> OrderLineId.create "OrderLineId" |> Result.mapError ValidationError
        let! productCode =
            unvalidatedOrderLine.ProductCode
            |> ProductCode.create "ProductCode"
            |> Result.mapError ValidationError
        let checkedProductCode = productCode |> toProductCode checkProductCodeExists
        let quantity = unvalidatedOrderLine.Quantity |> toOrderQuantity productCode
        
        let validatedOrderLine : ValidatedOrderLine = {
            OrderLineId = orderLineId
            ProductCode = productCode
            Quantity = quantity }
        return validatedOrderLine
    }

let validateOrder =
    fun checkProductCodeExists checkAddressExists (unvalidatedOrder: UnvalidatedOrder) ->
        result {
            let! orderId = unvalidatedOrder.OrderId |> OrderId.create |> Result.mapError ValidationError
            let! customerInfo = unvalidatedOrder.CustomerInfo |> toCustomerInfo
            let! shippingAddress =
                unvalidatedOrder.ShippingAddress
                |> checkAddressExists
                |> toAddress
            let! billingAddress =
                unvalidatedOrder.BillingAddress
                |> checkAddressExists
                |> toAddress
            
            let orderLines = unvalidatedOrder.OrderLines |> List.map (toValidatedOrderLine checkProductCodeExists)
            let validatedOrder = {
                OrderId = orderId
                CustomerInfo = customerInfo
                ShippingAddress = shippingAddress
                BillingAddress = billingAddress
                Lines = orderLines
            }
            return validatedOrder
        }
        
// Pricing

let toPricedOrderLine getProductPrice (line: ValidatedOrderLine) =
    let qty = line.Quantity |> OrderQuantity.value
    let price = line.ProductCode |> getProductPrice
    let linePrice = price |> Price.multiply qty
    {
        OrderLineId = line.OrderLineId
        ProductCode = line.ProductCode 
        Quantity = line.Quantity
        LinePrice = linePrice
    }

let priceOrder: PriceOrder =
    fun getProductPrice validatedOrder ->
        let lines = validatedOrder.Lines |> List.map (toPricedOrderLine getProductPrice)
        let amountToBill =
            lines
            |> List.map (fun line -> line.LinePrice) |> BillingAmount.sumPrices
        Ok {
            OrderId = validatedOrder.OrderId
            CustomerInfo = validatedOrder.CustomerInfo
            ShippingAddress = validatedOrder.ShippingAddress
            BillingAddress = validatedOrder.BillingAddress
            Lines = lines
            AmountToBill = amountToBill
        }

let priceOrderAdapted getProductPrice input =
    input
    |> priceOrder getProductPrice
    |> Result.mapError PlaceOrderError.Pricing

// Acknowledgment

let acknowledgeOrder: AcknowledgeOrder =
    fun createOrderAcknowledgmentLetter sendOrderAcknowledgment pricedOrder ->
        let letter = createOrderAcknowledgmentLetter pricedOrder
        let acknowledgment = {
            EmailAddress = pricedOrder.CustomerInfo.EmailAddress
            Letter = letter
        }
        
        match sendOrderAcknowledgment acknowledgment with
        | Sent ->
            let event = {
                EmailAddress = pricedOrder.CustomerInfo.EmailAddress
                OrderId = pricedOrder.OrderId
            }
            Some event
        | NotSent -> None
        
// Create events

let createBillingEvent placedOrder =
    let billingAmount = placedOrder.AmountToBill |> BillingAmount.value
    if billingAmount > 0M then
        let order = {
            OrderId = placedOrder.OrderId
            BillingAddress = placedOrder.BillingAddress
            AmountToBill = placedOrder.AmountToBill
        }
        Some order
    else
        None
        
let createEvents: CreateEvents =
    fun pricedOrder acknowledgmentEventOpt ->
        let events1 = pricedOrder |> PlaceOrderEvent.OrderPlaced |> List.singleton
        let events2 = acknowledgmentEventOpt |> Option.map PlaceOrderEvent.AcknowledgementSent |> listOfOption
        let events3 = pricedOrder |> createBillingEvent |> Option.map PlaceOrderEvent.BillableOrderPlaced |> listOfOption
        [
            yield! events1
            yield! events2
            yield! events3
        ]
