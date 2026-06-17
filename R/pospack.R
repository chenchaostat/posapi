#' @title Probability of Success (Package Method)
#' @description
#' Calculating probability of success using LongCART method in oncology clinical trials.
#'
#' @param type Type of the endpoint. "surv" for survival.
#' @param nsamples Number of samples.
#' @param null.value The specified value under null hypothesis.
#' @param alternative Direction of alternate hypothesis. "greater" or "less".
#' @param D Total number of events at final analysis.
#' @param a a = 1.
#' @param hr.prior Mean value of prior distribution for HR.
#' @param D.prior Prior D value.
#' @param succ.crit "trial" or "clinical".
#' @param alpha.final Alpha at final analysis.
#'
#' @returns A list containing PoS results.
#' @export
#'
#' @examples
#' \dontrun{
#' pospack(type="surv", nsamples=2, null.value=1, alternative="less",
#'         D=197, a=1, hr.prior=0.667, D.prior=9999999,
#'         succ.crit="trial", alpha.final=0.025)
#' }
pospack <- function(type = "surv",
                    nsamples = 2,
                    null.value = 1,
                    alternative = "less",
                    D = 197,
                    a = 1,
                    hr.prior = 0.667,
                    D.prior = 9999999,
                    succ.crit = "trial",
                    alpha.final = 0.025) {

  base_url <- "https://pos-api-server.onrender.com"
  endpoint <- paste0(base_url, "/pospack")

  body <- list(
    type = type,
    nsamples = nsamples,
    null.value = null.value,
    alternative = alternative,
    D = D,
    a = a,
    hr.prior = hr.prior,
    D.prior = D.prior,
    succ.crit = succ.crit,
    alpha.final = alpha.final
  )

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

  if (httr::status_code(response) != 200) {
    stop("API request failed with status code: ", httr::status_code(response), call. = FALSE)
  }

  result <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(result)

  return(result)
}
