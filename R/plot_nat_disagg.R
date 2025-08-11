#' Create bar graph of dhs indicator results
#'
#' @param df input dataframe
#' @param indicator dhs indicador id to visualize
#' @param survey_year survey year for desired indicator results
#' @param breakdown indicator disaggregation type
#' @param palette applied color palette
#' @param type value type (percentage vs absolute)
#' @param reverse_ord sorting order for good results to bad
#' @param info whether to include title, subtitle, and caption (default TRUE)
#'
#' @return a bar graph with dhs indicator results
#' @export
#'
#' @examples
#' \dontrun{
#' df <- plot_nat_disagg()
#' }

plot_nat_disagg <- function(df, indicator, survey_year = 2022, breakdown = "Region",
                            palette = "rocket", type = "Percent", reverse_ord = FALSE,
                            info = TRUE) {

  df <- df |>
    dplyr::filter(IndicatorId == indicator,
                  SurveyYear == survey_year,
                  CharacteristicCategory == breakdown)

  if (type == "Percent") {
    df <- df |>
      dplyr::mutate(Value = ifelse(is.na(Value), NA, Value / 100),
                    CIHigh = ifelse(CIHigh == "", NA, CIHigh / 100),
                    CILow  = ifelse(CILow  == "", NA, CILow  / 100))
  } else if (type == "Absolute") {
    df <- df |>
      dplyr::mutate(Value = ifelse(is.na(Value), NA, Value),
                    CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                    CILow  = ifelse(CILow  == "", NA, CILow))
  }

  df <- janitor::clean_names(df)

  survey_source <- df |> dplyr::pull(survey_type) |> unique()
  indicator_label <- df |> dplyr::pull(indicator) |> unique()
  publication_url <- df |> dplyr::pull(publication_url) |> unique()
  indicator_definition <- df |> dplyr::pull(definition) |> unique()

  p <- df |>
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(characteristic_label, value, .desc = reverse_ord),
                                 y = value, fill = value)) +
    viridis::scale_fill_viridis(option = palette, direction = -1, end = .8) +
    ggplot2::geom_col(alpha = .75) +
    glitr::si_style_xgrid() +
    ggplot2::coord_flip() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      plot.title = ggplot2::element_text(size = 14, vjust = 3),
      plot.subtitle = ggplot2::element_text(size = 8, vjust = 6, color = "grey60"),
      plot.caption = ggplot2::element_text(color = "grey50", size = 7, vjust = 1),
      panel.spacing = grid::unit(.75, "cm"),
      axis.text.x = ggplot2::element_text(size = 9, angle = 0, vjust = 1, hjust = .5),
      legend.position = "none",
      legend.direction = "vertical",
      legend.title = ggplot2::element_blank()
    )

  if (type == "Percent") {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(value, 1)),
                         size = 3, hjust = -.25, vjust = .5) +
      ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = 0.1))
  } else if (type == "Absolute") {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label = value),
                         size = 3, hjust = -.25, vjust = .5) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1))
  }

  # Always set axis labels; add title/subtitle/caption only if info = TRUE
  p <- p + ggplot2::labs(x = "", y = "")

  if (isTRUE(info)) {
    p <- p +
      ggplot2::labs(
        title = glue::glue("{indicator_label} ({survey_source} {survey_year})"),
        subtitle = glue::glue("{indicator_definition}"),
        caption = glue::glue(
          "Source: {survey_source} {survey_year}\nhttps://www.statcompiler.com/\n{publication_url}"
        )
      )
  }

  return(p)
}
