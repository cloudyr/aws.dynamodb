print.aws_dynamodb_table <- function(x, ...) {
    cat(sprintf("Dynamo DB Table: %s\n", x$TableName))
    cat(sprintf("TableId: %s\n", x$TableId))
    cat(sprintf("TableArn: %s\n", x$TableArn))
    cat(sprintf("CreationDateTime: %s\n", as.POSIXct(x$CreationDateTime, origin = "1970-01-01")))
    cat(sprintf("Status: %s\n", x$TableStatus))
    cat(sprintf("Throughput: %d (read), %d (write)\n", x$ProvisionedThroughput$ReadCapacityUnits, x$ProvisionedThroughput$WriteCapacityUnits))
    cat(sprintf("Size (Bytes): %s\n", x$TableSizeBytes))
    if (length(x$KeySchema) == 1L) {
        cat(sprintf("Primary Key: %s\n", x$KeySchema[[1L]]$AttributeName))
    } else if (length(x$KeySchema) == 2L) {
        cat(sprintf("Composite Primary Key: %s, %s\n", x$KeySchema[[1L]]$AttributeName, x$KeySchema[[2L]]$AttributeName))
    }
    ats <- paste0(unlist(lapply(x$AttributeDefinitions, function(at) {
        sprintf("%s (%s)", at$AttributeName, at$AttributeType)
    })), collapse = ", ")
    cat(strwrap(paste0("Attributes: ", ats), exdent = 2L), "\n")
    cat(sprintf("ItemCount: %s\n", x$ItemCount))
    invisible(x)
}
