#' Fisheriescape path to CEU, summarize trips within fishing areas.
#'
#' See Table 2 in "Building a Fisheriescape: mapping the threat of marine wildlife entanglement in vertical fishing lines in the Gulf of St. Lawrence, Canada".
#'
#' @param df A data frame
#' @param gear.type Must be either 'trap' or 'non-trap'. Required.
#' @param group.cols Names of columns of grouping variables. Needs to include the trip id and fishing area columns.
#' @param gear.col Name of gear column to summarize. Required.
#' @param hour.col Name of hours column to summarize. Required.
#' @param day.col Name of day column to summarize. Required if gear.type is non-trap.
#' @import dplyr
#' @examples
#' \dontrun{
#' trips<-fs_summarize_trips(df=ziff,
#'                           gear.type="trap",
#'                           group.cols=c('fishery.name','gear.name','trip.id','fleet'),
#'                           gear.col='gear.amount',
#'                           hour.col='hours.fished')
#' }
#' @export
fs_summarize_trips<-function(df,
                    gear.type=NULL,
                    group.cols=NULL,
                    gear.col=NULL,
                    hour.col=NULL,
                    day.col=NULL){


  df=as.data.frame(df)
  #////////////////////////////////////////////////////////////////////
  `%!in%` <- Negate(`%in%`)

  if(is.null(gear.type)|gear.type%!in%c('trap','non.trap')   ) {
    stop("gear.type must be either 'trap' 'or non.trap'")
    }


  if(gear.type=='non.trap'&& (is.null(gear.col)|is.null(hour.col)|is.null(day.col))){
    stop("gear.col, hour.col, and day.col must be specified for non-trap fisheries")
    }


  if(gear.type=='trap'&& (is.null(gear.col)|is.null(hour.col))){
    stop("gear.col and hour.col must be specified for trap fisheries")
  }
  #////////////////////////////////////////////////////////////////////
  # index group.cols
  #col.index<-which(names(df)%in%group.cols)
  #col.names <- names(df)[col.index]


  #////////////////////////////////////////////////////////////////////
  # appease R CMD check
  GEAR=NULL
  HRS=NULL
  DAYS=NULL
  df$GEAR<-df[,gear.col]
  #df[which(is.na(df$GEAR)),'GEAR']<-0 #to get around dplyr summarize  no non-missing arguments to max, because na.rm=TRUE not working
  df$HRS<-df[,hour.col]
  if(gear.type=='non-trap'){df$DAYS<-df[,day.col]}

  #str(df)
  #summary(df)
  #head(df)
  #////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////
  # PROCESS NON-TRAP FISHERIES ----
  if(gear.type=='non.trap'){
    suppressWarnings(
    trips<-df|>
      group_by(across(all_of(group.cols)))|>
      dplyr::mutate(sum.gear = sum(GEAR),
                    max.hours = max(HRS,na.rm=TRUE),
                    av.days = mean(DAYS, na.rm=TRUE))|>
      ungroup()|>
      dplyr::mutate_all(~ifelse(is.nan(.), NA, .))
    )

    trips=as.data.frame(trips)
    trips[trips == -Inf] <- NA

    # change any zeros to NA
    index<-which(trips$sum.gear==0)
    if(length(index)>0){trips[index,'sum.gear']<-NA}

    index<-which(trips$max.hours==0)
    if(length(index)>0){trips[index,'max.hours']<-NA}

    index<-which(trips$av.days==0)
    if(length(index)>0){trips[index,'av.days']<-NA}
  }

  #trips[which(trips$trip.id=='100503;2013-06-01;2013-06-01'),]
  #trips[which(trips$trip.id=='100503;2013-08-10;2013-08-10'),]

  #////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////
  # PROCESS TRAP FISHERIES ----
  if(gear.type=='trap'){
    df$HRS<-24
    df$DAYS<-NA

   # suppressWarnings(
      trips<-df|>
        group_by(across(all_of(group.cols)))|>
        dplyr::mutate(sum.gear = sum(GEAR),
                      max.hours = max(HRS,na.rm=TRUE))|>
        ungroup()|>
        dplyr::mutate_all(~ifelse(is.nan(.), NA, .))
    #)

    trips=as.data.frame(trips)
    trips[trips == -Inf] <- NA

    # change any zeros to NA
    index<-which(trips$sum.gear==0)
    if(length(index)>0){trips[index,'sum.gear']<-NA}

    index<-which(trips$max.hours==0)
    if(length(index)>0){trips[index,'max.hours']<-NA}

  }


  #////////////////////////////////////////////////////////////////////
    return(trips)
  }
