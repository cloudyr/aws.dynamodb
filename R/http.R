#' @title Execute Dynamo DB API Request
#' @description This is the workhorse function to execute calls to the Dynamo DB API.
#' @param verb A character string specifying an HTTP verb for the operation.
#' @param region A character string containing an AWS region. If missing, the default \dQuote{us-east-1} is used.
#' @param key A character string containing an AWS Access Key ID. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string containing an AWS Secret Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token Optionally, a character string containing an AWS temporary Session Token. See \code{\link[aws.signature]{locate_credentials}}.
#' @param ... Additional arguments passed to \code{\link[httr]{GET}}.
#' @return If successful, a named list. Otherwise, a data structure of class \dQuote{aws-error} containing any error message(s) from AWS and information about the request attempt.
#' @details This function constructs and signs an Dynamo DB API request and returns the results thereof, or relevant debugging information in the case of error.
#' @author Thomas J. Leeper
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_xml as_list
#' @importFrom aws.signature signature_v4_auth
#' @export
dynamoHTTP <- 
function(
  verb = "GET",
  body = NULL,
  target,
  region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
  key = NULL, 
  secret = NULL, 
  session_token = NULL,
  ...
) {
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    url <- paste0("dynamodb.",region,".amazonaws.com")
    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "dynamodb",
           verb = verb,
           action = "/",
           query_args = NULL,
           canonical_headers = list(host = paste0("dynamodb.",region,".amazonaws.com"),
                                    `x-amz-date` = d_timestamp),
           request_body = if (length(body)) jsonlite::toJSON(body, auto_unbox = TRUE) else "",
           key = key, 
           secret = secret,
           session_token = session_token)
    headers <- list(`x-amz-date` = d_timestamp, 
                    `x-amz-content-sha256` = Sig$BodyHash,
                    `x-amz-target` = target,
                    Authorization = Sig$SignatureHeader)
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    if (verb == "GET") {
        r <- GET(url, H, body = body, encode = "json", ...)
    } else if (verb == "POST") {
        r <- POST(url, H, body = body, encode = "json", ...)
    } else if (verb == "POST") {
        r <- PUT(url, H, body = body, encode = "json", ...)
    } else if (verb == "DELETE") {
        r <- DELETE(url, H, encode = "json", ...)
        if (!http_error(r)) {
            return(TRUE)
        }
    }
    cont <- content(r, "text", encoding = "UTF-8")
    if (http_error(r)) {
        warn_for_status(r)
        h <- headers(r)
        out <- try(structure(jsonlite::fromJSON(cont), headers = h, class = "aws_error"))
        if (inherits(out, "try-error")) {
            out <- xml2::as_list(xml2::read_xml(cont))
        }
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- try(jsonlite::fromJSON(cont, simplifyDataFrame = FALSE))
        if (inherits(out, "try-error")) {
            out <- xml2::as_list(xml2::read_xml(cont))
        }
    }
    return(out)
}
