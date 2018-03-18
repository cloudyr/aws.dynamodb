#' @title Delete a Dynamo DB Table
#' @description Delete a Dynamo DB table
#' @param table A character string specifying the table name, or an object of class \dQuote{aws_dynamodb_table}.
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @return A list.
#' @export
delete_table <- function(table, ...) {
    bod <- list(TableName = get_tablename(table))
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.DeleteTable", ...)
    structure(out$TableDescription, class = "aws_dynamodb_table")
}
