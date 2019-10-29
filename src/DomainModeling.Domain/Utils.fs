module DomainModeling.Domain.Utils

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>