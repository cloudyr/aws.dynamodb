get_tablename <- function(x) {
    UseMethod("get_tablename")
}

get_tablename.default <- function(x) {
    x
}

get_tablename.character <- function(x) {
    x
}

get_tablename.aws_dynamodb_table <- function(x) {
    x$TableName
}


map_attributes <- function(item) {
    item_formatted <- list()
    for (i in seq_along(item)) {
        if (is.null(item[[i]])) {
            item_formatted[[i]] <- list(NULL = TRUE)
        } else if (is.list(item[[i]])) {
            if (any(names(item[[i]]) %in% "")) {
                item_formatted[[i]] <- list(L = unname(item[[i]]))
            } else {
                item_formatted[[i]] <- list(M = item[[i]])
            }
        } else if (is.raw(item[[i]])) {
            item_formatted[[i]] <- list(B = jsonlite::base64_enc(item[[i]]))
        } else if (is.logical(item[[i]])) {
            item_formatted[[i]] <- list(BOOL = item[[i]])
        } else if (is.numeric(item[[i]])) {
            if (length(item[[i]]) == 1L) {
                item_formatted[[i]] <- list(N = item[[i]])
            } else {
                item_formatted[[i]] <- list(NS = item[[i]])
            }
        } else {
            if (length(item[[i]]) == 1L) {
                item_formatted[[i]] <- list(S = as.character(item[[i]]))
            } else {
                item_formatted[[i]] <- list(SS = as.character(item[[i]]))
            }
        }
    }
    names(item_formatted) <- names(item)
    return(item_formatted)
}
