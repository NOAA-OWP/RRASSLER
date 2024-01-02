#' @title URL tester
#' @description A function to test if I get a response from a URL
#' @param x the URL to test
#' @param non_2xx_return_value PARAM_DESCRIPTION, Default: FALSE
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @return returns TRUE if the url exists
#' @family helper
#' @details gratefully pilfered from https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
#' @examples
#' #EXAMPLE1
#' some_urls <-
#' c(
#'   "https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r",
#'   "http://jim.is/a/bad/programmer/",
#'   "https://github.com/NOAA-OWP/inundation-mapping"
#' )
#' data.frame(
#'   exists = sapply(some_urls, url_exists, USE.NAMES = FALSE),
#'   some_urls,
#'   stringsAsFactors = FALSE
#' ) %>% tibble::as_tibble() %>% print()
#' @seealso
#'  \code{\link[httr]{HEAD}}, \code{\link[httr]{GET}}, \code{\link[httr]{status_code}}
#' @rdname util_url
#' @export
#' @importFrom httr HEAD GET status_code
#'

url_exists <-
  function(x,
           non_2xx_return_value = FALSE,
           is_quiet = FALSE) {
    # sinew::moga(file.path(getwd(),"R/util_url.R"),overwrite = TRUE)
    # devtools::document()
    # pkgdown::build_site(new_process=FALSE)
    # devtools::load_all()

    # x = "http://jim.is/a/bad/programmer/"
    # non_2xx_return_value = FALSE
    # is_quiet = FALSE

    ## -- Start --
    suppressPackageStartupMessages({require("httr", quietly = FALSE, warn.conflicts = FALSE)})
    capture_error <- function(code,
                              otherwise = NULL,
                              quiet = TRUE) {
      tryCatch(
        list(result = code, error = NULL),
        error = function(e) {
          if (!quiet)
            message("Error: ", e$message)

          list(result = otherwise, error = e)
        },
        interrupt = function(e) {
          stop("Terminated by user", call. = FALSE)
        }
      )
    }

    safely <- function(.f,
                       otherwise = NULL,
                       quiet = TRUE) {
      function(...)
        capture_error(.f(...), otherwise, quiet)
    }

    sHEAD <- safely(httr::HEAD)
    sGET <- safely(httr::GET)

    # Try HEAD first since it's lightweight
    # res <- sHEAD(x, ...)
    res <- sHEAD(x)

    if (is.null(res$result) ||
        ((httr::status_code(res$result) %/% 200) != 1)) {
      # res <- sGET(x, ...)
      res <- sGET(x)

      if (is.null(res$result))
        return(NA) # or whatever you want to return on "hard" errors

      if (((httr::status_code(res$result) %/% 200) != 1)) {
        if (!is_quiet)
          warning(
            sprintf(
              "Requests for [%s] responded but without an HTTP status code in the 200-299 range",
              x
            )
          )
        return(non_2xx_return_value)
      }

      return(TRUE)

    } else {
      return(TRUE)
    }

  }
