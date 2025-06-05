#' Fisheriescape pivot table 1 in path to CEU calculations.
#'
#' @param df A data frame
#' @param keep.cols Names of columns in df to retain in output, if different than 'trip.id','fleet', 'gear.col','hour.col, or 'day.col'
#' @param gear.col Name of gear column to summarize. Required.
#' @param hour.col Name of hours column to summarize. Optional.
#' @param day.col Name of day column to summarize. Optional.
#' @import dplyr
#' @export
fs_pivot1<-function(df,
                    keep.cols,
                    gear.col,
                    hour.col=NULL,
                    day.col=NULL){

  # appease R CMD check
  trip.id=NULL
  fleet=NULL
  GEAR=NULL
  HRS=NULL
  DAYS=NULL


  '%!in%' <- function(x,y)!('%in%'(x,y))
  if(gear.col%!in%names(df)){{stop('\r Missing gear.col',call. = FALSE)}}

  df$GEAR<-df[,gear.col]
  df[which(is.na(df$GEAR)),'GEAR']<-0 #to get around dplyr summarize  no non-missing arguments to max, because na.rm=TRUE not working

  # gear only ----
  if(is.null(day.col)&is.null(hour.col)){
  pivot1<-df|>
    dplyr::group_by(trip.id,fleet)|>
    dplyr::mutate(sum.gear = sum(GEAR, na.rm=TRUE))

  index<-which(pivot1$sum.gear==0)
  if(length(index)>0){pivot1[index,'sum.gear']<-NA}

  COLS<-c('trip.id','fleet',keep.cols,'sum.gear')
  COLS<-unique(COLS)
  pivot1<-pivot1[,COLS]
  pivot1<-distinct(pivot1)

  }

  # gear and hours ----
  if(is.null(day.col)&!is.null(hour.col)){
    df$HRS<-df[,hour.col]
    df[which(is.na(df$HRS)),'HRS']<-0 #to get around dplyr summarize  no non-missing arguments to max, because na.rm=TRUE not working
    pivot1<-df|>
      dplyr::group_by(trip.id,fleet)|>
      dplyr::mutate(sum.gear = sum(GEAR, na.rm=TRUE),
                       max.hours = max(HRS, na.rm=TRUE))

    index<-which(pivot1$sum.gear==0)
    if(length(index)>0){pivot1[index,'sum.gear']<-NA}
    index<-which(pivot1$max.hours==0)
    if(length(index)>0){pivot1[index,'max.hours']<-NA}

    COLS<-c('trip.id','fleet',keep.cols,'sum.gear','max.hours')
    COLS<-unique(COLS)
    pivot1<-pivot1[,COLS]
    pivot1<-distinct(pivot1)
  }

  # gear and hours and days ----
  if(!is.null(day.col)&!is.null(hour.col)){
    df$HRS<-df[,hour.col]
    df[which(is.na(df$GEAR)),'GEAR']<-0 #to get around dplyr summarize  no non-missing arguments to max, because na.rm=TRUE not working
    df[which(is.na(df$HRS)),'HRS']<-0 #to get around dplyr summarize  no non-missing arguments to max, because na.rm=TRUE not working
    df$DAYS<-df[,day.col]
    df[which(df$DAYS==0),'DAYS']<-NA
    pivot1<-df|>
      dplyr::group_by(trip.id,fleet)|>
      dplyr::mutate(sum.gear = sum(GEAR, na.rm=TRUE),
                       max.hours = max(HRS, na.rm=TRUE),
                       av.days = mean(DAYS, na.rm=TRUE))
    index<-which(pivot1$sum.gear==0)
    if(length(index)>0){pivot1[index,'sum.gear']<-NA}
    index<-which(pivot1$max.hours==0)
    if(length(index)>0){pivot1[index,'max.hours']<-NA}
    index<-which(pivot1$av.days==0)
    if(length(index)>0){pivot1[index,'av.days']<-NA}

    COLS<-c('trip.id','fleet',keep.cols,'sum.gear','max.hours','av.days')
    COLS<-unique(COLS)
    pivot1<-pivot1[,COLS]
    pivot1<-distinct(pivot1)
  }


  message("Grouped by 'trip.id', 'fleet'")
  message("Duplicate rows removed")
  message("Before proceeding to pivot 2, use function `fs_fill_col` to fill in missing values")
  return(pivot1)
}

