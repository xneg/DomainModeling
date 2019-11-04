module internal DomainModeling.Domain.PlaceOrderWorkflow.Internal

open DomainModeling.Domain.Api
open DomainModeling.Domain.Utils
open DomainModeling.Domain.Primitives

type CheckProductCodeExists = ProductCode -> bool

type AddressValidationError = Undefined
type CheckedAddress = Undefined
type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress, AddressValidationError>
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
        
let validateOrder: ValidateOrder =
    fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
        
        let orderId = unvalidatedOrder.OrderId |> OrderId.create
        let customerInfo = unvalidatedOrder.CustomerInfo |> toCustomerInfo
        let shippingAddress = unvalidatedOrder.ShippingAddress |> toAddress
        
        {
            OrderId = orderId
        }     
