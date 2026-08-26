#' Fisheriescape path to CEU, summarize fishing areas within each time period
#'
#' See Table 2 in "Building a Fisheriescape: mapping the threat of marine wildlife entanglement in vertical fishing lines in the Gulf of St. Lawrence, Canada".
#' @param df The dataframe after running `fs_summarize_vessels`
#' @param gear.type Must be either 'trap' or non.trap'.
#' @param week.col Name of week column.
#' @param year.col Name of year column.
#' @param fishing.area.col Name of fishing area column.
#' @param also.grp Optional additional columns to group by. E.g., fishery, gear
#' @import dplyr
#' @export
fs_summarize_fishing_areas<-function(df,
                               gear.type=NULL,
                               week.col=NULL,
                               year.col=NULL,
                               fishing.area.col=NULL,
                               also.grp=NULL){
  # appease R CMD check
  gear=NULL
  hours=NULL
  days=NULL

  #////////////////////////////////////////////////////////////////////
  `%!in%` <- Negate(`%in%`)

  if(is.null(gear.type)|gear.type%!in%c('trap','non.trap')   ) {
    stop("gear.type must be either 'trap' 'or non.trap'")
  }

  if(is.null(gear.type)|is.null(week.col)|is.null(year.col)|is.null(fishing.area.col)   ) {
    stop("gear.type, week.col, year.col, and fishing.area.col must all be specified")
  }
  #////////////////////////////////////////////////////////////////////
  #GROUP.COLS<-c('fleet','year','sw')
  #GROUP.COLS<-c(vessel.col,GROUP.COLS)

  GROUP.COLS<-c(fishing.area.col,year.col,week.col)
  if(!is.null(also.grp)){
    GROUP.COLS<-c(also.grp,GROUP.COLS)
  }
  #////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////
  # PROCESS NON-TRAP FISHERIES ----
  if(gear.type=='non.trap'){
    suppressWarnings(
      fareas<-df|>
        dplyr::group_by(dplyr::across(any_of(GROUP.COLS)))|>
        dplyr::summarise(total.gear = sum(gear),
                         soak.time = mean(hours),
                         days = mean(days))
    )

  }


  #////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////
  # PROCESS TRAP FISHERIES ----
  if(gear.type=='trap'){
    suppressWarnings(
      fareas<-df|>
        dplyr::group_by(dplyr::across(any_of(GROUP.COLS)))|>
        dplyr::summarise(total.gear = sum(gear),
                         soak.time = mean(hours))
    )

  }

  #////////////////////////////////////////////////////////////////////
  return(fareas)

}
