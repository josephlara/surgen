#' Create a list of indicators based on the level1 field in the dhs_indicators function
#'
#' @param level_1 level1 indicators wanted.  If "all" then all indicators are returned.  The default is all
#'
#' @return a list of indicators based on level1
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- create_indicator_list()}

create_indicator_list <- function(level_1 = "all"){

  all_indicators <- rdhs::dhs_indicators()

  if("all" %in% level_1){
    indicator_id <- all_indicators |>
      dplyr::select(indicator_id) |>
      dplyr::pull()
    return(indicator_id)

  }

  else{

    indicators <- all_indicators |>
      dplyr::filter(Level1 %in% level_1) |>
      dplyr::select(IndicatorId) |>
      dplyr::pull()  # Convert to vector

    return(indicators)

  }

}
