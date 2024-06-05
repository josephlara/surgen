#' Create gt table of dhs indicator results and upper/lower bound
#'
#' @param df input dataframe
#' @param indicator dhs indicador id to visualize
#' @param survey_year survey year for desired indicator results
#' @param breakdown indicator disaggregation type
#' @param type value type (percentage vs absolute)
#' @param reverse_ord sorting order for good results to bad
#'
#' @return a gt table with dhs indicator results and upper and lower bound
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- tbl_nat_disagg()}

tbl_nat_disagg <- function(df, indicator, survey_year = 2022, breakdown = "Region", type = "Percent", reverse_ord = FALSE) {

  df <- df |>
    dplyr::filter(IndicatorId == indicator,
                  SurveyYear == survey_year,
                  CharacteristicCategory == breakdown)

  if (reverse_ord == FALSE) {

    df <- df |>

      dplyr::arrange(desc(Value))

  } else if (reverse_ord == TRUE) {

    df <- df |>

      dplyr::arrange(Value)

  }

  survey_source <- df |>
    dplyr::pull(SurveyType) |>
    unique()

  survey_year <- df |>
    dplyr::pull(SurveyYear) |>
    unique()

  indicator_label <- df |>
    dplyr::pull(Indicator) |>
    unique()

  df |>
    # dplyr::arrange(desc(Value)) |>
    dplyr::mutate(Value = Value / 100) |>
    dplyr::relocate(SurveyType, .after = "CILow") |>
    dplyr::relocate(SurveyYear, .before = "Value") |>
    gt::gt() |>
    gt::fmt_percent(columns = Value,
                    decimals = 0) |>
    gt::sub_missing(missing_text = ".") |>
    gt::cols_hide(columns = c("CountryName", "DHS_CountryCode", "SurveyId", "Level1", "Indicator", "IndicatorId",
                              "ByVariableLabel", "PublicationURL",  "SurveyYear",
                              "SurveyType", "CharacteristicCategory")) |>
    gt::cols_label(CharacteristicLabel = breakdown, #change this to breakdown
                   Value = "Value",
                   CILow = "Lower Bound",
                   CIHigh = "Upper Bound") |>
    gt::cols_align(align = "left", columns = 1) |>
    gt::tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = trolley_grey_light,
          weight = px(2)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = c()
        )
      )
    ) |>
    gt::tab_header(
      title = glue("{indicator_label} ({survey_year})")) |>
    gt::tab_source_note(
      source_note = gt::md(glue("Source:  {survey_source} {survey_year} \n
                                 https://www.statcompiler.com/"))) |>
    gt::tab_options(
      source_notes.font.size = px(9)) |>
    gtExtras::gt_theme_nytimes()

}
