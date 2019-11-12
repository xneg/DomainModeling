module DomainModeling.Domain.PlaceOrderWorkflow

open DomainModeling.Domain.Api
open DomainModeling.Domain.PlaceOrderWorkflow.Steps

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
        let validatedOrder =
            unvalidatedOrder.Data //??
            |> validateOrder checkOrderExists checkAddressExists
        let pricedOrder = validatedOrder |> priceOrder getProductPrice
        let acknowledgmentOption =
            pricedOrder
            |> acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment
        let events = createEvents pricedOrder acknowledgmentOption
        events

