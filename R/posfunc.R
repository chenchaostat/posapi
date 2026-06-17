#' @title Probability of Success (Formula Method)
#' @description
#' Calculating probability of success using formula method in oncology clinical trials.
#'
#' @param hr_true True hazard ratio which you assumed.
#' @param hr_threshold Hazard ratio threshold. It will be used to calculate Pr(hr < hr_threshold).
#' @param n_events PFS/OS events planned.
#'
#' @returns Prints Pr(hr < hr_threshold) to console.
#' @export
#'
#' @examples
#' \dontrun{
#' posfunc(hr_true = 0.75, hr_threshold = 0.85, n_events = 220)
#' }
posfunc <- function(hr_true = 0.75,
                    hr_threshold = 0.85,
                    n_events = 220) {

  # API endpoint (your deployed server)
  base_url <- "https://pos-api-server.onrender.com"
  endpoint <- paste0(base_url, "/posfunc")

  # Build request body
  body <- list(
    hr_true = hr_true,
    hr_threshold = hr_threshold,
    n_events = n_events
  )

  # Call API
  response <- tryCatch({
    httr::POST(
      url = endpoint,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::content_type_json(),
      encode = "raw"
    )
  }, error = function(e) {
    stop("Unable to connect to the API server. Please check your internet connection.\n",
         "Error: ", e$message, call. = FALSE)
  })

  # Check response status

  if (httr::status_code(response) != 200) {
    stop("API request failed with status code: ", httr::status_code(response), call. = FALSE)
  }

  # Parse result
  result <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(result)

  cat(result$message, "\n")
  invisible(result$probability)
}
