module DomainModeling.Domain.Utils // like static class

open System

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>

type Command<'data> = {
    Data: 'data
    Timestamp: DateTime
    UserId: string
}

let predicateToPassthru errorMsg f x =
    if f x then
        x
    else
        failwith errorMsg
        
let listOfOption opt =
    match opt with
    | Some x -> [x]
    | None -> []