module internal DomainModeling.Domain.PlaceOrderWorkflow.Internal

open DomainModeling.Domain.Api
open DomainModeling.Domain.Primitives
open DomainModeling.Domain.Utils

// Validation

type CheckedAddress = CheckedAddress of UnvalidatedAddress

type ValidationError = Undefined
type AddressValidationError = Undefined

type ValidateOrder =
    CheckProductCodeExists
        -> CheckAddressExists
        -> UnvalidatedOrder
        -> ValidatedOrder // AsyncResult<ValidatedOrder, ValidationError list>
and
    CheckProductCodeExists = ProductCode -> bool
and
    CheckAddressExists = UnvalidatedAddress -> CheckedAddress //AsyncResult<CheckedAddress, AddressValidationError>

// Pricing

type PricingError = Undefined
        
type PriceOrder =
    GetProductPrice
        -> ValidatedOrder
        -> PricedOrder //Result<PricedOrder, PricingError>
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
let toCustomerInfo (customer:UnvalidatedCustomerInfo) : CustomerInfo =
    let firstName = customer.FirstName |> String50.create "FirstName"
    let lastName = customer.LastName |> String50.create "LastName"
    let emailAddress = customer.EmailAddress |> EmailAddress.create "EmailAddress"
    
    let customerInfo = {
        Name = {FirstName = firstName; LastName = lastName}
        EmailAddress = emailAddress
    }
    customerInfo

let toAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress =
    let checkedAddress = checkAddressExists unvalidatedAddress
    let (CheckedAddress checkedAddress) = checkedAddress

    
    let addressLine1 = checkedAddress.AddressLine1 |> String50.create "AddressLine1"
    let addressLine2 = checkedAddress.AddressLine2 |> String50.create "AddressLine2"
    let addressLine3 = checkedAddress.AddressLine3 |> String50.create "AddressLine3"
    let addressLine4 = checkedAddress.AddressLine4 |> String50.create "AddressLine4"
    let city = checkedAddress.City |> String50.create "City"
    let zipCode = checkedAddress.ZipCode |> ZipCode.create "ZipCode"
    
    let address: Address = {
        AddressLine1 = addressLine1
        AddressLine2 = Some(addressLine2)
        AddressLine3 = Some(addressLine3)
        AddressLine4 = Some(addressLine4)
        City = city
        ZipCode = zipCode
    }
    address

let toProductCode (checkProductCodeExists: CheckProductCodeExists) productCode =
    let checkProduct =
        let errorMsg = sprintf "Invalid: %A" productCode
        predicateToPassthru errorMsg checkProductCodeExists
        
    productCode
    |> ProductCode.create "ProductCode"
    |> checkProduct

let toOrderQuantity productCode quantity =
    match productCode with
    | Widget _ ->
        quantity |> int |> UnitQuantity.create "Quantity" |> OrderQuantity.Unit
    | Gizmo _ ->
        quantity |> decimal |> KilogramQuantity.create "Quantity" |> OrderQuantity.Kilos

let toValidatedOrderLine checkProductCodeExists (unvalidatedOrderLine: UnvalidatedOrderLine) =
    let orderLineId = unvalidatedOrderLine.OrderLineId |> OrderLineId.create "OrderLineId"
    let productCode = unvalidatedOrderLine.ProductCode |> toProductCode checkProductCodeExists
    let quantity = unvalidatedOrderLine.Quantity |> toOrderQuantity productCode
    
    let validatedOrderLine : ValidatedOrderLine = {
        OrderLineId = orderLineId
        ProductCode = productCode
        Quantity = quantity
    }
    validatedOrderLine

let validateOrder: ValidateOrder =
    fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
        
        let orderId = unvalidatedOrder.OrderId |> OrderId.create
        let customerInfo = unvalidatedOrder.CustomerInfo |> toCustomerInfo
        let shippingAddress = unvalidatedOrder.ShippingAddress |> toAddress checkAddressExists
        let billingAddress = unvalidatedOrder.BillingAddress |> toAddress checkAddressExists
        let orderLines = unvalidatedOrder.OrderLines |> List.map (toValidatedOrderLine checkProductCodeExists)
        {
            OrderId = orderId
            CustomerInfo = customerInfo
            ShippingAddress = shippingAddress
            BillingAddress = billingAddress
            Lines = orderLines
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
        {
            OrderId = validatedOrder.OrderId
            CustomerInfo = validatedOrder.CustomerInfo
            ShippingAddress = validatedOrder.ShippingAddress
            BillingAddress = validatedOrder.BillingAddress
            Lines = lines
            AmountToBill = amountToBill
        }

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
