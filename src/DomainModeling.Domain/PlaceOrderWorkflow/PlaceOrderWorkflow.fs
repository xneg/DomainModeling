module DomainModeling.Domain.PlaceOrderWorkflow

open DomainModeling.Domain.Result
open DomainModeling.Domain.Api
open DomainModeling.Domain.PlaceOrderWorkflowSteps

//let checkOrderExists: CheckProductCodeExists = failwith "Not implemented"
//let checkAddressExists: CheckAddressExists = failwith "Not implemented"
//let getProductPrice: GetProductPrice = failwith "Not implemented"
//
//let createOrderAcknowledgmentLetter : CreateOrderAcknowledgmentLetter = failwith "Not implemented"
//let sendOrderAcknowledgment : SendOrderAcknowledgment = failwith "Not implemented"

let placeOrder
    checkOrderExists
    checkAddressExists
    getProductPrice
    createOrderAcknowledgmentLetter
    sendOrderAcknowledgment
    : PlaceOrderWorkflow =
    fun unvalidatedOrder ->
        result {
            let! validatedOrder =
                unvalidatedOrder.Data // ??
                |> validateOrder checkOrderExists checkAddressExists
                |> Result.mapError PlaceOrderError.Validation
            let! pricedOrder =
                validatedOrder
                |> priceOrder getProductPrice
                |> Result.mapError PlaceOrderError.Pricing
            let acknowledgmentOption =
                pricedOrder
                |> acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment
            let events = createEvents pricedOrder acknowledgmentOption
            return events
        }
        
