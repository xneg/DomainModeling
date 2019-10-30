module DomainModeling.Domain.Utils // like static class

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>