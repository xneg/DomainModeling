namespace rec DomainModeling.Domain.PlaceOrderWorkflow
open DomainModeling.Domain.Api
open DomainModeling.Domain.Primitives

type ValidatedOrderLine = Undefined

type ValidatedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    OrderLines: ValidatedOrderLine list
}

type CustomerInfo = {
    Name: PersonalName
    EmailAddress: EmailAddress
}

type PersonalName = {
    FirstName : String50
    LastName : String50
}

type Address = Undefined

type PricedOrderLine = Undefined
type PricedOrder = Undefined

type Order =
    | Unvalidated of UnvalidatedOrder
    | Validated of ValidatedOrder
    | Priced of PricedOrder
