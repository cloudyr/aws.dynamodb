#' @title Get Dynamo DB Tables
#' @description Get a description of a Dynamo DB tables
#' @param table A character string specifying the table name, or an object of class \dQuote{aws_dynamodb_table}.
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @return A list.
#' @references
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_GetTable.html}{API Guide: GetTable}
#' @export
get_table <- function(table, ...) {
    bod <- list(TableName = get_tablename(table))
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.DescribeTable", ...)
    if (length(out$Table)) {
        structure(out$Table, class = "aws_dynamodb_table")
    } else {
        return(NULL)
    }
}
