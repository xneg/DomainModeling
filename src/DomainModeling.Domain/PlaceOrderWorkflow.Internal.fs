module DomainModeling.Domain.PlaceOrderWorkflow.Internal

open DomainModeling.Domain.Api
open DomainModeling.Domain.Utils
open DomainModeling.Domain.Primitives

type CheckProductCodeExists = ProductCode -> bool

type AddressValidationError = Undefined
type CheckedAddress = Undefined
type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress, AddressValidationError>

type ValidateOrder =
    CheckProductCodeExists
        -> CheckAddressExists
        -> UnvalidatedOrder
        -> AsyncResult<ValidatedOrder, ValidationError list> 