namespace rec DomainModeling.Domain.Api

open DomainModeling.Domain.Utils
open DomainModeling.Domain.Primitives

// input
type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
    BillingAddress: UnvalidatedAddress
    OrderLines: UnvalidatedOrderLine list
}
and UnvalidatedCustomerInfo = {
    FirstName: string
    LastName: string
    EmailAddress: string
}
and UnvalidatedAddress = {
    AddressLine1: string
    AddressLine2: string
    AddressLine3: string
    AddressLine4: string
    City: string
    ZipCode: string
}
and UnvalidatedOrderLine = {
    OrderLineId: string
    ProductCode: string
    Quantity: string
}

type PlaceOrderCommand = Command<UnvalidatedOrder>

// output
type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgementSent of OrderAcknowledgmentSent
and
    OrderPlaced = PricedOrder
and
    BillableOrderPlaced = {
    OrderId: OrderId
    BillingAddress: Address
    AmountToBill: BillingAmount
}
and
    OrderAcknowledgmentSent = {
    OrderId : OrderId
    EmailAddress : EmailAddress 
}

type PlaceOrderError =
    | Validation of ValidationError
    | Pricing of PricingError
and
    ValidationError = ValidationError of string
and
    PricingError = Undefined


type PlaceOrderWorkflow = PlaceOrderCommand -> Result<PlaceOrderEvent list, PlaceOrderError> //AsyncResult<PlaceOrderEvent list, PlaceOrderError>

type ValidatedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    Lines: ValidatedOrderLine list
}
and CustomerInfo = {
    Name: PersonalName
    EmailAddress: EmailAddress
}
and PersonalName = {
    FirstName : String50
    LastName : String50
}
and Address = {
    AddressLine1 : String50
    AddressLine2 : String50 option
    AddressLine3 : String50 option
    AddressLine4 : String50 option
    City : String50
    ZipCode : ZipCode
}
and ValidatedOrderLine = {
    OrderLineId: OrderLineId
    ProductCode: ProductCode
    Quantity: OrderQuantity
}

type PricedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    Lines: PricedOrderLine list
    AmountToBill: BillingAmount
}
and PricedOrderLine =  {
    OrderLineId : OrderLineId 
    ProductCode : ProductCode 
    Quantity : OrderQuantity
    LinePrice : Price
}

// пока не очень ясно, для чего он здесь
type Order =
    | Unvalidated of UnvalidatedOrder
    | Validated of ValidatedOrder
    | Priced of PricedOrder