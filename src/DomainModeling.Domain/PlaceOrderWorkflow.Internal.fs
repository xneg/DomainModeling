module internal DomainModeling.Domain.PlaceOrderWorkflow.Internal

open DomainModeling.Domain.Api
open DomainModeling.Domain.Primitives
open DomainModeling.Domain.Primitives

type CheckProductCodeExists = ProductCode -> bool

type AddressValidationError = Undefined

type CheckedAddress = CheckedAddress of UnvalidatedAddress

//type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress, AddressValidationError>
type CheckAddressExists = UnvalidatedAddress -> CheckedAddress
type ValidationError = Undefined

//type ValidateOrder =
//    CheckProductCodeExists
//        -> CheckAddressExists
//        -> UnvalidatedOrder
//        -> AsyncResult<ValidatedOrder, ValidationError list>

type ValidateOrder =
    CheckProductCodeExists
        -> CheckAddressExists
        -> UnvalidatedOrder
        -> ValidatedOrder

type Price = Undefined        
type GetProductPrice = ProductCode -> Price

type PricingError = Undefined

type PriceError =
    GetProductPrice
        -> ValidatedOrder
        -> Result<PricedOrder, PricingError>
       

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

let toProductCode (checkProductCodeExists: CheckProductCodeExists) productCode : ProductCode =
    failwith "Not implemented"

let toOrderQuantity productCode quantity =
    match productCode with
    | Widget _ ->
        quantity |> int |> UnitQuantity.create "Quantity" |> OrderQuantity.Unit
    | Gizmo _ ->
        quantity |> decimal |> KilogramQuantity.create "Quantity" |> OrderQuantity.Kilos

let toValidatedOrderLine checkProductCodeExists unvalidatedOrderLine =
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
            OrderLines = orderLines
        }     
