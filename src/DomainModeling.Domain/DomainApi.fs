namespace rec DomainModeling.Domain.Api

open System
open DomainModeling.Domain.Utils
open DomainModeling.Domain.Primitives

type SomeType = SomeType of int

type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
    BillingAddress: UnvalidatedAddress
    OrderLines: UnvalidatedOrderLine list
}

type UnvalidatedCustomerInfo = {
    FirstName: string
    LastName: string
    EmailAddress: string
}

type UnvalidatedAddress = {
    AddressLine1: string
    AddressLine2: string
    AddressLine3: string
    AddressLine4: string
    City: string
    ZipCode: string
}

type UnvalidatedOrderLine = {
    OrderLineId: string
    ProductCode: string
    Quantity: string
}

type Command<'data> = {
    Data: 'data
    Timestamp: DateTime
    UserId: string
}

type PlaceOrderCommand = Command<UnvalidatedOrder>

type OrderPlaced = Undefined
type BillableOrderPlaced = Undefined
type OrderAcknowledgmentSent = {
    OrderId : OrderId
    EmailAddress : EmailAddress 
}

type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgementSent of OrderAcknowledgmentSent
    
type PlaceOrderError = Undefined

type PlaceOrderWorkflow = PlaceOrderCommand -> AsyncResult<PlaceOrderEvent list, PlaceOrderError>

type ValidatedOrderLine = {
    OrderLineId: OrderLineId
    ProductCode: ProductCode
    Quantity: OrderQuantity
}

type ValidatedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    Lines: ValidatedOrderLine list
}

type CustomerInfo = {
    Name: PersonalName
    EmailAddress: EmailAddress
}

type PersonalName = {
    FirstName : String50
    LastName : String50
}

type Address = {
    AddressLine1 : String50
    AddressLine2 : String50 option
    AddressLine3 : String50 option
    AddressLine4 : String50 option
    City : String50
    ZipCode : ZipCode
}

type PricedOrderLine =  {
    OrderLineId : OrderLineId 
    ProductCode : ProductCode 
    Quantity : OrderQuantity
    LinePrice : Price
    }
type PricedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    Lines: PricedOrderLine list
    AmountToBill: BillingAmount
}

type Order =
    | Unvalidated of UnvalidatedOrder
    | Validated of ValidatedOrder
    | Priced of PricedOrder