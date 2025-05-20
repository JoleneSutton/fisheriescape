#' Fisheriescape pivot table 3 in path to CEU calculations.
#'
#' @param pivot2 The output from function `fs_pivot2`
#' @param group.cols Names of columns to group by.
#' @param id.col Name of column that is the vessel or licence or individual specifier.
#' @param trap.fishery Is it a trap fishery? Default is 'yes'.
#' @import dplyr
#' @export
fs_pivot3<-function(pivot2,
                 group.cols,#must include 'year' and 'sw'
                 id.col,#cfv or licence depending on fishery
                 trap.fishery
                 ){

  message("'group.cols' must include year and sw, and either cfv or licence")

  pivot2$ID<-pivot2[,id.col]

  if(trap.fishery=='yes'){
  ## gear only ----
  if(isTRUE('gear'%in%names(pivot2))&isFALSE('hours'%in%names(pivot2))&isFALSE('days'%in%names(pivot2))){
      pivot3<-pivot2|>
      dplyr::group_by(dplyr::across(any_of(group.cols)))|>
      dplyr::summarise(n.vessels = n_distinct(ID),
                       total.gear = sum(gear))
  }


  # gear and hours ----
  if(isTRUE('gear'%in%names(pivot2))&isTRUE('hours'%in%names(pivot2))&isFALSE('days'%in%names(pivot2))){
    pivot3<-pivot2|>
      dplyr::group_by(dplyr::across(any_of(group.cols)))|>
      dplyr::summarise(n.vessels = n_distinct(ID),
                       total.gear = sum(gear),
                       soak.time = max(hours))
  }

  # gear and hours and days ----
  if(isTRUE('gear'%in%names(pivot2))&isTRUE('hours'%in%names(pivot2))&isTRUE('days'%in%names(pivot2))){
    pivot3<-pivot2|>
      dplyr::group_by(dplyr::across(any_of(group.cols)))|>
      dplyr::summarise(n.vessels = n_distinct(ID),
                       total.gear = sum(gear),
                       soak.time = max(hours),
                       days = mean(days))
  }}







  if(trap.fishery!='yes'){
    ## gear only ----
    if(isTRUE('gear'%in%names(pivot2))&isFALSE('hours'%in%names(pivot2))&isFALSE('days'%in%names(pivot2))){
      pivot3<-pivot2|>
        dplyr::group_by(dplyr::across(any_of(group.cols)))|>
        dplyr::summarise(n.vessels = n_distinct(ID),
                         total.gear = sum(gear))
    }


    # gear and hours ----
    if(isTRUE('gear'%in%names(pivot2))&isTRUE('hours'%in%names(pivot2))&isFALSE('days'%in%names(pivot2))){
      pivot3<-pivot2|>
        dplyr::group_by(dplyr::across(any_of(group.cols)))|>
        dplyr::summarise(n.vessels = n_distinct(ID),
                         total.gear = sum(gear),
                         soak.time = mean(hours))
    }


    # gear and hours and days ----
    if(isTRUE('gear'%in%names(pivot2))&isTRUE('hours'%in%names(pivot2))&isTRUE('days'%in%names(pivot2))){
      pivot3<-pivot2|>
        dplyr::group_by(dplyr::across(any_of(group.cols)))|>
        dplyr::summarise(n.vessels = n_distinct(ID),
                         total.gear = sum(gear),
                         soak.time = mean(hours),
                         days = mean(days))
    }
  }



  return(pivot3)
}
