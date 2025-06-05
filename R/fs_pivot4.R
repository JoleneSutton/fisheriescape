#' Fisheriescape not really a pivot, but should be done after pivot table 3 in path to CEU calculations.
#'
#' @param df1 The ziff data frame.
#' @param trap.fishery Is it a trap fishery? Default is 'yes'.
#' @param pivot3 The output from fs_pivot3
#' @param fleet.col Name of fleet column. Should match between df and pivot3
#' @param prov.col Currently NULL
#' @param area.note.col Currently NULL
#' @import dplyr
#' @export
fs_pivot4<-function(df1, #ziff
                    trap.fishery,
                    pivot3, #output from fs_pivot3
                    fleet.col, #should match between df and pivot3
                    prov.col=NULL,
                    area.note.col=NULL
                    ){

  # appease R CMD check
  fleet=NULL
  sw=NULL
  dateland=NULL


  message("WARNING: This draft function does not yet allow flexibility in `group_by`. Grouping in step 'pivot.season' is done only with year and fleet. Use with CAUTION")

  df1$FLEET<-df1[,fleet.col]
  df3<-as.data.frame(pivot3)
  df3$FLEET<-df3[,fleet.col]


  #//////////////////////////////////////////////////////----
  # SEASONS----
  #//////////////////////////////////////////////////////
  season.ziff<-df1
  season.ziff<-droplevels(season.ziff)
  FLEET<-season.ziff$FLEET

  # need to add some if statements to allow for province and area_note**************************************************
  #province<-season.ziff$prov # grouping specific to certain fisheries.#future function to allow for flexibility here
  #area_note<-season.ziff$area_note # grouping specific to certain fisheries.#future function to allow for flexibility here
  season.ziff$trip.id<-gsub("-","",season.ziff$trip.id)
  landed<-as.numeric(do.call(rbind, strsplit(season.ziff$trip.id,";"))[,2])
  caught<-as.numeric(do.call(rbind, strsplit(season.ziff$trip.id,";"))[,3])
  year<-as.numeric(substr(landed, start = 1, stop = 4))
  season<-cbind.data.frame(year,FLEET,caught,landed)#,province,area_note) #future function to allow for flexibility here

  pivot.season<-as.data.frame(season |>
                                dplyr::group_by(year,FLEET)|> #future function to allow for flexibility here
                                dplyr::summarise(season.start = min(caught,na.rm=T),    #fishery start date
                                                 season.stop = max(landed,na.rm=T))  ) #fishery end date


  #//////////////////////////////////////////////////////
  start.week<-substring(ISOweek::ISOweek(as.Date(as.character(pivot.season$season.start),format="%Y%m%d")),7,8)
  stop.week<-substring(ISOweek::ISOweek(as.Date(as.character(pivot.season$season.stop),format="%Y%m%d")),7,8)
  start.weekday<-ISOweek::ISOweekday(as.Date(as.character(pivot.season$season.start),format="%Y%m%d"))
  stop.weekday<-ISOweek::ISOweekday(as.Date(as.character(pivot.season$season.stop),format="%Y%m%d"))

  pivot.season<-cbind.data.frame(pivot.season,start.week,start.weekday,stop.week,stop.weekday)

  pivot.season$prop.start.week<-(8-pivot.season$start.weekday)/7
  pivot.season$prop.stop.week<-pivot.season$stop.weekday/7
  #//////////////////////////////////////////////////////
  SEASONS<-pivot.season

  to.find.start<-paste(SEASONS$FLEET,SEASONS$year,SEASONS$start.week, sep="_")
  to.replace.start<-SEASONS$prop.start.week
  original<-paste(df3$FLEET,df3$year,df3$sw,sep="_")
  x.start<-eclectic::find_replace(original,to.find.start,to.replace.start,no.match = 1)

  to.find.stop<-paste(SEASONS$FLEET,SEASONS$year,SEASONS$stop.week, sep="_")
  to.replace.stop<-SEASONS$prop.stop.week
  x.stop<-eclectic::find_replace(original,to.find.stop,to.replace.stop,no.match = 1)

  index.start<-which(x.start[,2]!="1")
  index.stop<-which(x.stop[,2]!="1")

  ceu<-df3
  ceu<-ceu[,-which(names(ceu)=='FLEET')]

  if(trap.fishery=='yes'){
  ceu$prop.week.fished<-1
  ceu[index.start,'prop.week.fished']<-as.numeric(x.start[index.start,2])
  ceu[index.stop,'prop.week.fished']<-as.numeric(x.stop[index.stop,2])
  }

  if(trap.fishery!='yes'){
    int.step<-df1|>
      group_by(fleet,year,sw)|>
      summarize(days.fished = n_distinct(dateland)/7)

    ceu<-left_join(pivot3,int.step)

    ceu$prop.week.fished<-ceu$days.fished*ceu$days

    index<-which(ceu$prop.week.fished>1)
    if(length(index)>0){ceu[which(ceu$prop.week.fished>1),'prop.week.fished']<-1}
    }
  #names(ceu)[which(names(ceu)=='FLEET')]<-names(df1)[which(names(df1)%in%fleet.col)]

  return(ceu)

  #//////////////////////////////////////////////////////
  # could include following code specific to each fishery #future function to allow for flexibility here
  #ceu$num.lines<-ceu$sum.av.gear#/ceu$traps.trawl*2
  #ceu$soak.time<-24
  #ceu$rope.soak.time<-ceu$soak.time/24
  #ceu$magnitude<-ceu$num.lines
  #ceu$intensity<-ceu$magnitude*ceu$rope.soak.time
  #ceu$ceu = ceu$intensity*ceu$prop.week.fished
  #head(as.data.frame(ceu))
  #summary(ceu)

  #ceu2<-ceu|>
  # group_by(FLEET,year,sw)|>
  #summarise(ceu = sum(ceu),
  #         max.sum.av.gear=max(sum.av.gear))

  ## mean ceu
  #(base<-length(unique(ceu2$year)))
  #av.ceu<-ceu2|>
  #  group_by(sw,FLEET)|>
  #  summarise(av.ceu = mean(ceu))
  #head(av.ceu)
}
