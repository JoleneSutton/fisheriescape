#' Fisheriescape path to CEU, calculate the proportion of each week fished
#'
#' See Table 2 in "Building a Fisheriescape: mapping the threat of marine wildlife entanglement in vertical fishing lines in the Gulf of St. Lawrence, Canada".
#' @param df The original data frame of fishing records that includes columns dateland and ctchdate, each formatted as YYYY-MM-DD
#' @param fish.area.summary The dataframe resulting from `fs_summarize_fishing_areas`
#' @param gear.type Must be either 'trap' or non.trap'.
#' @param week.col Name of week column. Names must match between df and fish.area.summary
#' @param fishing.area.col Name of fishing area column. Names must match between df and fish.area.summary
#' @param also.grp Optional additional columns to group by. E.g., fishery, gear. Names must match between df and fish.area.summary
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @export
fs_proportion_week_fished<-function(df=NULL,
                                    fish.area.summary=NULL,#results from function `fs_summarize_fishing_areas`
                                    gear.type=NULL,
                                    week.col=NULL,
                                    fishing.area.col=NULL,
                                    also.grp=NULL){

  # appease R CMD check
  dateland=NULL
  ctchdate=NULL
  date.end=NULL
  date.begin=NULL
  season=NULL
  weekday=NULL
  fleet=NULL
  year=NULL
  sw=NULL
  FLEET=NULL
  seasons=NULL

  #////////////////////////////////////////////////////////////////////
  `%!in%` <- Negate(`%in%`)

  if(is.null(df)   ) {
    stop("The raw data with colums for dateland and ctchdate must be specified.")
  }

  if(is.null(fish.area.summary)   ) {
    stop("The output from function `fs_summarize_fishing_areas` must be specified")
  }

  if(is.null(gear.type)|gear.type%!in%c('trap','non.trap')   ) {
    stop("gear.type must be either 'trap' 'or non.trap'")
  }

  if(is.null(week.col)|is.null(fishing.area.col)  ) {
    stop("week.col and fishing.area.col must be specified")
  }

  df$SW<-df[,week.col]
  df$FLEET<-df[,fishing.area.col]
  #fishing.area.col$SW<-fishing.area.col[,week.col]
  #fishing.area.col$FLEET<-fishing.area.col[,fishing.area.col]

  GROUP.COLS<-unique(c(fishing.area.col,week.col))
  if(!is.null(also.grp)){
    GROUP.COLS<-c(also.grp,GROUP.COLS)
  }

  SEASON.COLS<-GROUP.COLS
  index<-grep(week.col,SEASON.COLS)
  if(length(index)>0){SEASON.COLS<-SEASON.COLS[-index]}

  #////////////////////////////////////////////////////////////////////
  # non-trap fisheries----
  if(gear.type=='non.trap'){
    int.step<-df|>
      dplyr::group_by(dplyr::across(all_of(GROUP.COLS)))|>
      dplyr::summarize(days.fished = n_distinct(dateland)/7)
    nontrap<-dplyr::left_join(fish.area.summary,int.step)
    nontrap$prop.week.fished=NA
    nontrap[,'prop.week.fished']= nontrap[,'days.fished']*nontrap[,'days']
    index<-which(nontrap$prop.week.fished>1)
    if(length(index)>0){nontrap[which(nontrap$prop.week.fished>1),'prop.week.fished']<-1}
    keep.cols=c(GROUP.COLS,'total.gear','soak.time','prop.week.fished')
    nontrap=nontrap[,which(names(nontrap)%in%keep.cols)]
    return(nontrap)
  }

  #////////////////////////////////////////////////////////////////////
  # trap fisheries----
  if(gear.type=='trap'){

    seasons <- df |>
      dplyr::group_by(dplyr::across(all_of(SEASON.COLS)))|>

      dplyr:: summarise(
        date.begin = min(ctchdate, na.rm = TRUE),
        date.end = max(dateland, na.rm = TRUE),
        season.length=(date.end-date.begin)+1
      ) |>
      dplyr::ungroup()|>
      tidyr::pivot_longer(cols = starts_with("date."),
                   names_to = c("season"),
                   values_to=c('date'))|>
      dplyr::mutate(sw=substring(ISOweek::ISOweek(date),7,8),
                    weekday=ISOweek::ISOweekday(date))|>
      dplyr::mutate(
        prop = if_else(
          grepl("begin", season, ignore.case = TRUE),  # condition
          (8-weekday)/7,                                    # value if condition TRUE
          weekday/7                                          # value if FALSE
        )
      )


  #adjust for identical begin and end dates
  cut.cols=c('season.length'   ,  'season'  ,     'date','weekday')
  seasons<-seasons[,-which(names(seasons)%in%cut.cols)]

  seasons <- as.data.frame(seasons %>%
                                group_by(FLEET,year) %>%
                                mutate(has_duplicate = sw %in% sw[duplicated(sw) | duplicated(sw, fromLast = TRUE)]) %>%
                                ungroup())
  seasons[which(seasons$has_duplicate==TRUE),]
  index<-which(seasons$has_duplicate==TRUE)
  if(length(index)>0){
    seasons[which(seasons$has_duplicate==TRUE),'prop']<-1/7
  }
  seasons<-distinct(seasons)
  seasons<-seasons[,-ncol(seasons)]

  names(seasons)[which(names(seasons))=='FLEET']<-fishing.area.col

  fareas2<-left_join(fish.area.summary,seasons)

  #/////////////////////////////
  # proportion of week fished for other than first and last weeks
    trap=fareas2
    trap$prop.week.fished<-trap$prop
    index=which(is.na(trap$prop.week.fished))
    if(length(index)>0){
      trap[index,'prop.week.fished']<-1
    }
    keep.cols=c(GROUP.COLS,'total.gear','soak.time','prop.week.fished')
    trap=trap[,which(names(trap)%in%keep.cols)]
    trap=as.data.frame(trap)
    return(trap)
    }
}
