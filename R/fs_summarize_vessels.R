#' Fisheriescape path to CEU, summarize vessels (or licences) within each fishing area and time period
#'
#' See Table 2 in "Building a Fisheriescape: mapping the threat of marine wildlife entanglement in vertical fishing lines in the Gulf of St. Lawrence, Canada".
#' @param df The dataframe after running `fs_summarize_trips` and `fs_fill_missing`
#' @param gear.type Must be either 'trap' or non.trap'.
#' @param vessel.col Name of vessel or licence column.
#' @param week.col Name of week column.
#' @param year.col Name of year column.
#' @param fishing.area.col Name of fishing area column.
#' @param also.grp Optional additional columns to group by. E.g., fishery, gear
#' @import dplyr
#' @export
fs_summarize_vessels<-function(df,
                               gear.type=NULL,
                               vessel.col=NULL,
                               week.col=NULL,
                               year.col=NULL,
                               fishing.area.col=NULL,
                               also.grp=NULL){

  #////////////////////////////////////////////////////////////////////
  `%!in%` <- Negate(`%in%`)

  if(is.null(gear.type)|gear.type%!in%c('trap','non.trap')   ) {
    stop("gear.type must be either 'trap' 'or non.trap'")
  }

  if(is.null(gear.type)|is.null(vessel.col)|is.null(week.col)|is.null(year.col)|is.null(fishing.area.col)   ) {
    stop("gear.type, vessel.col, week.col, year.col, and fishing.area.col must all be specified")
  }
  #////////////////////////////////////////////////////////////////////
  #GROUP.COLS<-c('fleet','year','sw')
  #GROUP.COLS<-c(vessel.col,GROUP.COLS)

  GROUP.COLS<-c(vessel.col,fishing.area.col,year.col,week.col)
  if(!is.null(also.grp)){
    GROUP.COLS<-c(also.grp,GROUP.COLS)
  }
  #////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////
  # PROCESS NON-TRAP FISHERIES ----
  if(gear.type=='non.trap'){
    suppressWarnings(
      vessels<-df|>
        dplyr::group_by(dplyr::across(all_of(GROUP.COLS)))|>
        dplyr::summarise(gear = mean(sum.gear),
                         hours = max(max.hours),
                         days = mean(av.days))
    )

  }


  #////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////
  # PROCESS TRAP FISHERIES ----
  if(gear.type=='trap'){
    suppressWarnings(
      vessels<-df|>
        dplyr::group_by(dplyr::across(all_of(GROUP.COLS)))|>
        dplyr::summarise(gear = max(sum.gear),
                         hours = max(max.hours))
    )

  }

  #////////////////////////////////////////////////////////////////////
  return(vessels)

}
