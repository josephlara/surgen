#' Function to create a dataset from DHS_indicator()
#'
#' @param start_idx starting index of a group of indicators.  Grouped by a batch_size
#' @param batch_size number of indicators to be included in each batch
#' @param country countries - based on DHS_CountryCode
#' @param breakdown select: national, subnational, all, background
#' @param start start year of survey
#' @param end end year of survey
#'
#' @return a dataset with indicators
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_batch()}

process_batch <- function(start_idx, batch_size, country, breakdown, start, end) {
  end_idx <- min(start_idx + batch_size - 1, total_indicators)
  batch_indicators <- indicators[start_idx:end_idx]

  pull_indicator_df(indicator = batch_indicators,
                    country = country,
                    breakdown_default = breakdown,
                    start_year = start, # specify start year
                    end_year = end)   # specify end year
}
