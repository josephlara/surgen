#' Create gt table of DHS indicator results and bounds
#'
#' @param df input dataframe
#' @param indicator DHS indicator id to visualize
#' @param survey_year survey year for desired indicator results
#' @param breakdown indicator disaggregation type (e.g. 'Region', 'Education', 'Residence')
#' @param type value type ("Percent" vs "Absolute")
#' @param reverse_ord logical; if TRUE, sort ascending (bad → good); if FALSE, sort descending (good → bad)
#' @param info logical; include title, subtitle, and source note (default TRUE). If FALSE, these are omitted.
#'
#' @return A `gt` table showing DHS indicator results with upper and lower bounds.
#'   When `info = FALSE`, the table excludes the header and source note.
#' @export
#'
#' @examples
#' \dontrun{
#' # With header and source
#' tbl_nat_disagg(df, indicator = "CH_VACC_C_OP3", survey_year = 2022)
#'
#' # Minimal table (no header/source)
#' tbl_nat_disagg(df, indicator = "CH_VACC_C_OP3", survey_year = 2022, info = FALSE)
#' }
tbl_nat_disagg <- function(df, indicator, survey_year = 2022, breakdown = "Region",
                           type = "Percent", reverse_ord = FALSE, info = TRUE) {

  df <- df |>
    dplyr::filter(IndicatorId == indicator,
                  SurveyYear == survey_year,
                  CharacteristicCategory == breakdown)

  if (!reverse_ord) {
    df <- df |> dplyr::arrange(dplyr::desc(Value))
  } else {
    df <- df |> dplyr::arrange(Value)
  }

  survey_source <- df |> dplyr::pull(SurveyType) |> unique()
  survey_year   <- df |> dplyr::pull(SurveyYear) |> unique()
  indicator_label <- df |> dplyr::pull(Indicator) |> unique()
  indicator_definition <- df |> dplyr::pull(Definition) |> unique()

  gt_tbl <- df |>
    dplyr::mutate(Value = Value / 100) |>
    dplyr::relocate(SurveyType, .after = "CILow") |>
    dplyr::relocate(SurveyYear, .before = "Value") |>
    gt::gt() |>
    gt::fmt_percent(columns = Value, decimals = 0) |>
    gt::sub_missing(missing_text = ".") |>
    gt::cols_hide(columns = c(
      "CountryName", "DHS_CountryCode", "SurveyId", "Level1", "Indicator", "IndicatorId",
      "ByVariableLabel", "PublicationURL", "SurveyYear", "Definition", "TagIds",
      "SurveyType", "CharacteristicCategory"
    )) |>
    gt::cols_label(
      CharacteristicLabel = breakdown,
      Value = "Value",
      CILow = "Lower Bound",
      CIHigh = "Upper Bound"
    ) |>
    gt::cols_align(align = "left", columns = 1) |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("left"),
          color = trolley_grey_light,
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_body(columns = c())
      )
    )

  if (isTRUE(info)) {
    gt_tbl <- gt_tbl |>
      gt::tab_header(
        title = glue::glue("{indicator_label} ({survey_year})"),
        subtitle = indicator_definition
      ) |>
      gt::tab_source_note(
        source_note = gt::md(glue::glue(
          "Source: {survey_source} {survey_year}\nhttps://www.statcompiler.com/"
        ))
      ) |>
      gt::tab_options(source_notes.font.size = gt::px(9))
  }

  gt_tbl |>
    gtExtras::gt_theme_nytimes()
}
