module DomainModeling.Domain.Utils // like static class

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>

let predicateToPassthru errorMsg f x =
    if f x then
        x
    else
        failwith errorMsg