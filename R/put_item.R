#' @rdname put_item
#' @title Put an item
#' @description Put an item into a Dynamo DB database
#' @param table A character string specifying the table name, or an object of class \dQuote{aws_dynamodb_table}.
#' @param item A list of key-value pairs.
#' @param condition Optionally, a \dQuote{ConditionExpression} that determines whether the item is added. This can prevent overwriting. See \href{https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.ConditionExpressions.html}{User Guide: Condition Expressions}.
#' @param return_value A character string specifying whether to return previous values of the item.
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @return A list.
#' @references
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html}{API Guide: PutItem}
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_UpdateItem.html}{API Guide: UpdateItem}
#' @examples
#' \dontrun{
#'   tab <- create_table(
#'     table = "Music",
#'     attributes = list(Artist = "S"),
#'     primary_key = "Artist"
#'   )
#' 
#'   # put item
#'   put_item("Music", list(Artist = "No One You Know", SongTitle = "Call Me Today"))
#' 
#'   # cleanup
#'   delete_table(tab)
#' }
#' @export
put_item <-
function(
  table,
  item,
  condition = NULL,
  return_value = c("NONE", "ALL_OLD"),
  ...
) {
    # format items
    item_formatted <- map_attributes(item)
    
    return_value <- match.arg(return_value)
    bod <- list(TableName = get_tablename(table),
                ReturnValues = return_value,
                Item = item_formatted)
    if (!is.null(condition)) {
        bod$ConditionExpression <- condition
    }
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.PutItem", ...)
    if (return_value == "NONE") {
        return(NULL)
    } else {
        return(out)
    }
}

#' @rdname put_item
#' @export
update_item <-
function(
  table,
  item,
  return_value = c("NONE", "ALL_OLD"),
  ...
) {
    # format items
    item_formatted <- map_attributes(item)
    
    return_value <- match.arg(return_value)
    bod <- list(TableName = get_tablename(table),
                ReturnValues = return_value,
                Item = item_formatted)
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.UpdateItem", ...)
    if (return_value == "NONE") {
        return(NULL)
    } else {
        return(out)
    }
}
