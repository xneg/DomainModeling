namespace DomainModeling.Domain.PlaceOrderWorkflow

open DomainModeling.Domain.Api

type ValidatedOrderLine = Undefined

type ValidatedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
}