#' @title List Dynamo DB Backups
#' @description Retreive a paginated list of backup tables
#' @param marker A character string containing the first backup table to return (contained as an attribute in a previous call to this function)
#' @param n A number between 1 and 100 specifying the total number of tables to return
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @return A list of backup tables.
#' @export
list_tables <- function(marker = NULL, n = 100, ...) {
    stopifnot(n <= 100 & n >= 1)
    bod <- list(Limit = n)
    if (!is.null(marker)) {
        bod$ExclusiveStartBackupArn <- marker
    }
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.ListBackups", ...)
    structure(out$BackupSummaries, Marker = out$LastEvaluatedBackupArn)
}
