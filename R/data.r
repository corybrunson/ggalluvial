#' Influenza vaccination survey responses
#'
#' This data set is aggregated from three RAND American Life Panel (ALP) surveys
#' that asked respondents their probability of vaccinating for influenza. Their
#' responses were discretized to "Never" (0%), "Always" (100%), or "Sometimes"
#' (any other value). After merging, missing responses were coded as "Missing"
#' and respondents were grouped and counted by all three coded responses. The
#' pre-processed data were kindly contributed by Raffaele Vardavas, and the
#' complete surveys are freely available at the ALP website.
#' 
#' @keywords datasets
#' @format A data frame with 117 rows and 5 variables:
#' \describe{
#'   \item{`freq`}{number of respondents represented in each row}
#'   \item{`subject`}{identifier linking respondents across surveys}
#'   \item{`survey`}{survey designation from the ALP website}
#'   \item{`start_date`}{start date of survey}
#'   \item{`end_date`}{end date of survey}
#'   \item{`response`}{discretized probability of vaccinating for influenza}
#' }
#' @source \url{https://alpdata.rand.org/}
"vaccinations"

#' Students' declared majors across several semesters
#'
#' This data set follows the major curricula of 10 students across 8 academic
#' semesters. Missing values indicate undeclared majors. The data were kindly
#' contributed by Dario Bonaretti.
#' 
#' @name majors
#' @keywords datasets
#' @format A data frame with 80 rows and 3 variables:
#' \describe{
#'   \item{`student`}{student identifier}
#'   \item{`semester`}{character tag for odd-numbered semesters}
#'   \item{`curriculum`}{declared major program}
#' }
NULL
