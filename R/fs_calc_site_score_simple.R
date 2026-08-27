#' Calculates the fisheriescape site scores after all records have been assigned to a spatial reference grid.
#'
#' @param df A data frame with columns for fishing area id, year, week, and spatial reference grid cell id.
#' @param fishing.area.col The name of the column with fishing area ids.
#' @param year.col The name of the column with years.
#' @param week.col The name of the column with weeks.
#' @param grid.col The name of the column with spatial reference grid cell ids.
#' @import dplyr
#' @export
fs_calc_site_score_simple<-function(df, fishing.area.col=NULL, year.col=NULL, week.col=NULL, grid.col=NULL){

  counts=NULL
  site.score=NULL
  df.ss=NULL
  fa=NULL
  year=NULL
  sw=NULL
  grid=NULL
  sum.count.fa.yr.sw=NULL


  df$fa<-df[,fishing.area.col]
  df$year<-df[,year.col]
  df$sw<-df[,week.col]
  df$grid<-df[,grid.col]

  #/////////////////////////////////////////////////////////////////////////
  # Count points per grid cell per year per week ----
  counts<-df|>
    dplyr::group_by(fa,year,sw,grid)|>
    dplyr::summarise(count = dplyr::n())

  #/////////////////////////////////////////////////////////////////////////
  # Site score ----
  site.score<-counts|>
    dplyr::group_by(fa,year,sw)|>
    dplyr::mutate(sum.count.fa.yr.sw=sum(count),
                  ss=count/sum.count.fa.yr.sw)

  df.ss<-as.data.frame(site.score[,c(fishing.area.col, year.col, week.col, grid.col, 'ss')])

  return(df.ss)

}
