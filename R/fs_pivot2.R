#' Fisheriescape pivot table 2 in path to CEU calculations.
#'
#' @param pivot1 The output from function `fs_pivot1`
#' @param trap.fishery Is it a trap fishery? Default is 'yes'.
#' @param group.cols Names of columns to group by.
#' @import dplyr
#' @export
fs_pivot2<-function(pivot1,
                    trap.fishery='yes',
                    group.cols
                    ){
  # for a trap fishery, max of sum.gear and max of max.hours
  # for a longline or gillnet fishery, mean of sum.gear and mean of max.hours


  # appease R CMD check
  sum.gear=NULL
  max.hours=NULL
  av.days=NULL


  message("'group.cols' must include year, sw, and fleet, and either cfv or licence")
  group.cols = unique(c('year','sw','fleet', group.cols))

  if(trap.fishery=='yes'){
  ## gear only ----
  if(isTRUE('sum.gear'%in%names(pivot1))&isFALSE('max.hours'%in%names(pivot1))&isFALSE('av.days'%in%names(pivot1))){

    pivot2<-pivot1|>
      dplyr::group_by(dplyr::across(any_of(group.cols)))|>
      dplyr::summarise(gear = max(sum.gear))
  }

  # gear and hours ----
  if(isTRUE('sum.gear'%in%names(pivot1))&isTRUE('max.hours'%in%names(pivot1))&isFALSE('av.days'%in%names(pivot1))){
    pivot2<-pivot1|>
      dplyr::group_by(dplyr::across(any_of(group.cols)))|>
      dplyr::summarise(gear = max(sum.gear),
                       hours = max(max.hours))
  }

  # gear and hours and days ----
  if(isTRUE('sum.gear'%in%names(pivot1))&isTRUE('max.hours'%in%names(pivot1))&isTRUE('av.days'%in%names(pivot1))){
    pivot2<-pivot1|>
      dplyr::group_by(dplyr::across(any_of(group.cols)))|>
      dplyr::summarise(gear = max(sum.gear),
                       hours = max(max.hours),
                       days = mean(av.days))
  }
  }

  if(trap.fishery!='yes'){
    ## gear only ----
    if(isTRUE('sum.gear'%in%names(pivot1))&isFALSE('max.hours'%in%names(pivot1))&isFALSE('av.days'%in%names(pivot1))){

      pivot2<-pivot1|>
        dplyr::group_by(dplyr::across(any_of(group.cols)))|>
        dplyr::summarise(gear = mean(sum.gear))
    }

    # gear and hours ----
    if(isTRUE('sum.gear'%in%names(pivot1))&isTRUE('max.hours'%in%names(pivot1))&isFALSE('av.days'%in%names(pivot1))){
      pivot2<-pivot1|>
        dplyr::group_by(dplyr::across(any_of(group.cols)))|>
        dplyr::summarise(gear = mean(sum.gear),
                         hours = mean(max.hours))
    }

    # gear and hours and days ----
    if(isTRUE('sum.gear'%in%names(pivot1))&isTRUE('max.hours'%in%names(pivot1))&isTRUE('av.days'%in%names(pivot1))){
      pivot2<-pivot1|>
        dplyr::group_by(dplyr::across(any_of(group.cols)))|>
        dplyr::summarise(gear = mean(sum.gear),
                         hours = mean(max.hours),
                         days = mean(av.days))
    }
  }

  return(pivot2)
}

