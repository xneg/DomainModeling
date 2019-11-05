namespace rec DomainModeling.Domain.Api

open System
open DomainModeling.Domain.Utils

type SomeType = SomeType of int

type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
    BillingAddress: UnvalidatedAddress
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

type Command<'data> = {
    Data: 'data
    Timestamp: DateTime
    UserId: string
}

type PlaceOrderCommand = Command<UnvalidatedOrder>

type OrderPlaced = Undefined
type BillableOrderPlaced = Undefined
type OrderAcknowledgmentSent = Undefined

type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgementSent of OrderAcknowledgmentSent
    
type PlaceOrderError = Undefined

type PlaceOrderWorkflow = PlaceOrderCommand -> AsyncResult<PlaceOrderEvent list, PlaceOrderError>