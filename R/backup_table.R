#' @rdname create_table_backup
#' @title Backup a Dynamo DB Table
#' @description Create a backup of a Dynamo DB table, or restore a table from a backup
#' @param table A character string specifying the table name, or an object of class \dQuote{aws_dynamodb_table}.
#' @param backup For creating: a character string specifying the backup table name. For restoring: a backup table ARN.
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @return A list.
#' @seealso \code{\link{list_backups}}
#' @export
create_table_backup <- function(table, backup, ...) {
    bod <- list(TableName = get_tablename(table), BackupName = get_tablename(backup))
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.BackupTable", ...)
    if (length(out$BackupDetails)) {
        names(out$BackupDetails) <- sub("^Backup", "^Table", names(out$BackupDetails))
        structure(out$BackupDetails, class = "aws_dynamodb_table")
    } else {
        return(NULL)
    }
}

#' @rdname create_table_backup
#' @export
restore_table_backup <- function(table, backup, ...) {
    bod <- list(TargetTableName = get_tablename(table), BackupArn = backup)
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.RestoreTableFromBackup", ...)
    if (length(out$TableDescription)) {
        structure(out$TableDescription, class = "aws_dynamodb_table")
    } else {
        return(NULL)
    }
}
