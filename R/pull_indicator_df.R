#' Create an RDS dataset for Mozambique based on a given list of indicators and years
#'
#' @param indicator list of indicators
#' @param country country
#' @param breakdown_default indicator disaggregation
#' @param start_year earliest year for surveys
#' @param end_year latest year for surveys
#'
#' @return a dataset of indicators and years
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- pull_indicator_df()}

pull_indicator_df <- function(indicator, country = "MZ", breakdown_default = "all",
                              start_year, end_year) {
  tryCatch({
    df <- rdhs::dhs_data(indicatorIds = indicator,
                         countryIds = country,
                         breakdown = breakdown_default,
                         surveyYearStart = start_year,
                         surveyYearEnd = end_year)

    if (nrow(df) == 0) {
      warning("No records returned for the given query.")
      return(NULL)
    }

    df <- df |>
      dplyr::filter(IsPreferred == 1,# in testing phase

      ) |>
      dplyr::select(CountryName, DHS_CountryCode, SurveyType, SurveyId,
                    SurveyYear, Indicator, IndicatorId, CharacteristicCategory,
                    CharacteristicLabel, Value, CIHigh, CILow,
                    ByVariableLabel) |>
      dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                    CILow = ifelse(CILow == "", NA, CILow))

    indicators <- rdhs::dhs_indicators()

    #TODO  duplication in definitions - removed for now
    val_area <- indicators |>
      dplyr::select(IndicatorId, Level1) |>
      dplyr::distinct()

    df <- df |>
      dplyr::left_join(val_area, by = "IndicatorId")


    #Add publication URL from Surveys
    temp <- rdhs::dhs_publications() |>
      dplyr::filter(PublicationTitle == "Final Report") |>
      dplyr::select(SurveyId, PublicationURL) |>
      dplyr::distinct()

    df <- df |>
      dplyr::left_join(temp, by = "SurveyId")

    return(df)

  }, error = function(e) {
    errorMessage <- paste("An error occurred:", conditionMessage(e))
    warning(errorMessage)
    return(NULL)
  })
}
