#' @rdname create_table
#' @title Create/Update Dynamo DB Table
#' @description Create or update a Dynamo DB Table
#' @param table A character string specifying the table name.
#' @param attributes A named list of key-value pairs, where the key is the attribute name and value is one of \dQuote{S} (string), \dQuote{N} (number), or \dQuote{B} (binary). Users only need to specify the attribute(s) used for the primary key.
#' @param primary_key The primary key for the database. This should be the name of one of the attributes specified in \code{attributes}.
#" @param sort_key Optionally, a sort key for a \dQuote{composite primary key} specified in addition to \code{primary_key}.
#' @param read_capacity A number specifying read capacity units (i.e., number of reads per second). See References.
#' @param write_capacity A number specifying write capacity units (i.e., number of reads per second). See References.
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @references
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughput.html}{Provisioned Throughput}
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey}{Primary Key}
#' @examples
#' \dontrun{
#'   tab <- create_table(
#'     table = "Music",
#'     attributes = list(Artist = "S", SongTitle = "S"),
#'     primary_key = "Artist",
#'     sort_key = "SongTitle"
#'   )
#' 
#'   # cleanup
#'   delete_table(tab)
#' }
#' @export
create_table <- 
function(
  table,
  attributes,
  primary_key,
  sort_key = NULL,
  read_capacity = 5,
  write_capacity = 5,
  ...
) {
    bod <- list()
    
    # TableName
    stopifnot(nchar(table) >= 3 & nchar(table) <= 255)
    bod$TableName <- table
    
    # ProvisionedThroughput
    bod$ProvisionedThroughput <- list(ReadCapacityUnits = read_capacity,
                                      WriteCapacityUnits = write_capacity)
    
    # KeySchema
    if (is.null(sort_key)) {
        bod$KeySchema <- list(list(AttributeName = primary_key, KeyType = "HASH"))
    } else {
        bod$KeySchema <- list(
                            list(AttributeName = primary_key, KeyType = "HASH"),
                            list(AttributeName = sort_key, KeyType = "RANGE")
                         )
    }
    
    # AttributeDefinitions
    bod$AttributeDefinitions <- list()
    for (i in seq_along(attributes)) {
        bod$AttributeDefinitions[[i]] <- list(AttributeName = names(attributes)[i],
                                              AttributeType = toupper(substring(attributes[[i]][1], 1L, 1L)))
    }
    
    # execute request
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.CreateTable", ...)
    return(out)
    structure(out$TableDescription, class = "aws_dynamodb_table")
}

#' @rdname create_table
#' @export
update_table <- 
function(
  table,
  read_capacity = 5,
  write_capacity = 5,
  ...
) {
    bod <- list()
    
    # TableName
    bod$TableName <- get_tablename(table)
    
    # ProvisionedThroughput
    bod$ProvisionedThroughput <- list(ReadCapacityUnits = read_capacity,
                                      WriteCapacityUnits = write_capacity)
    
    # execute request
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.UpdateTable", ...)
    structure(out$TableDescription, class = "aws_dynamodb_table")
}
