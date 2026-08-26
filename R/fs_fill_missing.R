#' Fisheriescape path to CEU, fill NAs (replace NAs) after summarizing trips
#'
#' Replaces NAs with either group means or group maximums. See Table 2 in "Building a Fisheriescape: mapping the threat of marine wildlife entanglement in vertical fishing lines in the Gulf of St. Lawrence, Canada".
#' @param df The data frame produced by function `fs_summarize_trips`, for which duplicate rows have been removed.
#' @param gear.type Must be either 'trap' or 'non-trap'.
#' @param vessel.col Names of column of vessel (or licence) ids.
#' @param year.col Name of column of years.
#' @param week.col Name of column of weeks.
#' @param also.grp Names of other columns to group by. Optional. E.g., 'fishery', 'gear'
#' @return A data frame
#' @import dplyr
#'@examples
#'\dontrun{
#'trips<-fs_summarize_trips(df=ziff,
#'                       gear.type="trap",
#'                       group.cols=c('fishery.name','gear.name','trip.id','fleet'),
#'                       gear.col='gear.amount',
#'                       hour.col='hours.fished')
#'
#'# restrict columns and remove duplicate rows
#'COLS<-c('fishery.name','gear.name','fleet','trip.id',
#'        "cfv",'year','sw',
#'        'sum.gear','max.hours')
#'
#'trips2<-trips[,COLS]
#'trips2<-distinct(trips2)
#'
#'trips3<-fs_fill_missing(trips2,
#'                        gear.type='trap',
#'                        vessel.col='cfv',
#'                        year.col='year',
#'                        week.col='sw',
#'                        also.grp=c("fishery.name" ,"gear.name"))
#'
#'}
#' @export
fs_fill_missing<-function(df,
                             gear.type=NULL, #trap or non.trap
                             vessel.col=NULL,
                             year.col=NULL,
                             week.col=NULL,
                            also.grp=NULL){

    message("Did you remember to reduce columns and remove duplicate rows?")


    df=as.data.frame(df)

    #////////////////////////////////////////////////////////////////////
    `%!in%` <- Negate(`%in%`)

    if(is.null(gear.type)|gear.type%!in%c('trap','non.trap')   ) {
      stop("gear.type must be either 'trap' 'or non.trap'")
    }


    if(is.null(vessel.col)|is.null(year.col)|is.null(week.col)){
      stop("vessel.col, year.col, and week.col must be specified")
    }

    #////////////////////////////////////////////////////////////////////
    # appease R CMD check
    VESSEL=NULL
    YEAR=NULL
    WEEK=NULL
    df$VESSEL<-df[,vessel.col]
    df$YEAR<-df[,year.col]
    df$WEEK<-df[,week.col]


    #////////////////////////////////////////////////////////////////////
    GROUP.COLS<-list(c('VESSEL','YEAR','WEEK'),
                     c('VESSEL','YEAR'),
                     c('VESSEL'),
                     c('YEAR','WEEK'),
                     c('YEAR'))

    if(!is.null(also.grp)){
      GROUP.COLS<-lapply(GROUP.COLS, function(x) c(also.grp,x))
    }

    GROUP.COLS

    #/////////////////////////////////////////
    for(i in 1:length(GROUP.COLS)){
      # GEAR
      if(length(which(is.na(df$sum.gear)))>0){
        df2<-df|>
          dplyr::group_by(dplyr::across(all_of(unlist(GROUP.COLS[i]))))|>
          dplyr::summarise(dplyr::across(all_of('sum.gear'), \(x) .funs=mean(x, na.rm = TRUE)))

        #df2
        names(df2)[ncol(df2)]<-'val'
        df2[,ncol(df2)]<-round(df2[,ncol(df2)])
        tmp<-dplyr::left_join(df,df2)
        #tmp
        index=which(is.na(tmp[,'sum.gear']))
        tmp[index,]
        tmp[index,'sum.gear']<-tmp[index,'val']
        tmp[index,]
        df<-tmp[,-ncol(tmp)]
        df<-df |> mutate_all(function(x) ifelse(is.nan(x), NA, x))
      }
    }

    #/////////////////////////////////////////
    for(i in 1:length(GROUP.COLS)){
      # HOURS
      if(length(which(is.na(df$max.hours)))>0){
        df2<-df|>
          dplyr::group_by(dplyr::across(any_of(unlist(GROUP.COLS[i]))))|>
          dplyr::summarise(dplyr::across(any_of('max.hours'), \(x) .funs=mean(x, na.rm = TRUE)))

        #df2
        names(df2)[ncol(df2)]<-'val'
        df2[,ncol(df2)]<-round(df2[,ncol(df2)])
        tmp<-dplyr::left_join(df,df2)
        #tmp
        index=which(is.na(tmp[,'max.hours']))
        tmp[index,]
        tmp[index,'max.hours']<-tmp[index,'val']
        tmp[index,]
        df<-tmp[,-ncol(tmp)]
        df<-df |> mutate_all(function(x) ifelse(is.nan(x), NA, x))
      }
    }

    #/////////////////////////////////////////
    # DAYS
    if(gear.type=='non.trap'){
      for(i in 1:length(GROUP.COLS)){
        if(length(which(is.na(df$av.days)))>0){
          df2<-df|>
            dplyr::group_by(dplyr::across(any_of(unlist(GROUP.COLS[i]))))|>
            dplyr::summarise(dplyr::across(any_of('av.days'), \(x) .funs=mean(x, na.rm = TRUE)))

          #df2
          names(df2)[ncol(df2)]<-'val'
          df2[,ncol(df2)]<-round(df2[,ncol(df2)])
          tmp<-dplyr::left_join(df,df2)
          #tmp
          index=which(is.na(tmp[,'av.days']))
          tmp[index,]
          tmp[index,'av.days']<-tmp[index,'val']
          tmp[index,]
          df<-tmp[,-ncol(tmp)]
          df<-df |> mutate_all(function(x) ifelse(is.nan(x), NA, x))
        }
      }
    }

      #summary(df)
      #head(df)
      #/////////////////////////////////////////
    x=which(names(df)%in%c('VESSEL','YEAR','WEEK'))
    df=df[,-x]


    #////////////////////////////////////////////////////////////////////
  return(df)
}
