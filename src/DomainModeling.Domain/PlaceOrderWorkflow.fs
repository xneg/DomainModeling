namespace rec DomainModeling.Domain.PlaceOrderWorkflow
open DomainModeling.Domain.Api

type ValidatedOrderLine = Undefined

type ValidatedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    OrderLines: ValidatedOrderLine list
}

type OrderId = Undefined
type CustomerInfo = Undefined
type Address = Undefined

type PricedOrderLine = Undefined
type PricedOrder = Undefined

type Order =
    | Unvalidated of UnvalidatedOrder
    | Validated of ValidatedOrder
    | Priced of PricedOrder
