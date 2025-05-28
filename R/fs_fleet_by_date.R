#' Assign missing fleets based on nearest date within range for same cfv.
#'
#' @param df A data frame containing columns: cfv, year, fleet, dateland in format "%Y%m%d"
#' @param match.type Match with dates that are either 'exact', 'same.year', 'previous.year'. Default is 'exact' date. For 'same.year' and 'previous.year', 'num.days' must be specified.
#' @param num.days Plus and minus days around date to search.
#' @importFrom eclectic find_replace
#' @export
fs_fleet_by_date<-function(df,
                          match.type='exact',#exact, same.year, previous.year
                          num.days=NULL){

  message('THIS IS A DRAFT FUNCTION, USE WITH CAUTION!')
  message('using columns: fleet, dateland,...')

  df$key<-paste0(df$cfv,'-',df$year)
  df.assigned<-df[which(!is.na(df$fleet)),]
  df.unassigned<-df[which(is.na(df$fleet)),]


  # ////////////////////////////////////////////
  # Exact date ----
  # ////////////////////////////////////////////
  if(match.type=='exact'){

  for(i in 1:nrow(df.unassigned)){
    key<-df.unassigned[i,'key']
    missing.date<-df.unassigned[i,'dateland']
    matches<-which(df.assigned$key==key&df.assigned$dateland==missing.date)

    if(length(matches)>0){
      to.fill<-df.assigned[matches,'fleet']
      if(length(unique(to.fill))==1){
        df.unassigned[i,'fleet']<-unique(to.fill)}}
    }
    filled<-rbind(df.unassigned,df.assigned)
  }



  # ////////////////////////////////////////////
  # Within plus or minus N days of SAME year ----
  # ////////////////////////////////////////////
  if(match.type=='same.year'){

    N<-num.days
    index<-which(unique(df.unassigned[,'cfv'])%in%df.assigned[,'cfv'])
    check.boats<-unique(df.unassigned[,'cfv'])[index]
    index<-check.boats

    my.list<-list()
    for(i in 1:length(index)){
      boat<-index[i]
      out<-matrix(nrow=length(which(df.unassigned$cfv%in%index[i])),ncol=4)

      out[,1]<-boat
      cgeo<-df.assigned[which(df.assigned$cfv%in%boat),]
      cnogeo<-df.unassigned[which(df.unassigned$cfv%in%boat),]

      for(j in 1:nrow(cnogeo)){# loop each row; might be slow

        row<-cnogeo[j,]
        target.date<-as.Date(row$dateland,"%Y%m%d")

        prior.year <- as.POSIXlt(as.Date(row$dateland,"%Y%m%d"))
        prior.year$year <- prior.year$year-1
        prior.year <- as.Date(prior.year)

        #create date range limits
        start_date <- target.date-N
        end_date <- target.date+N

        compare.dates<-as.Date(cgeo$dateland,"%Y%m%d")
        sort(compare.dates)
        compare.dates<-compare.dates[which(compare.dates>=start_date&compare.dates<=end_date)]#within range
        closest.date<-compare.dates[which.min(abs(compare.dates - target.date))]#closest date

        if(length(closest.date)>0){
          POLYGON<-cgeo[which(cgeo$cfv%in%boat&as.Date(cgeo$dateland,"%Y%m%d")%in%closest.date),'fleet']

          if(length(unique(POLYGON))==1){
            out[j,2]<-as.character(target.date)
            out[j,3]<-as.character(closest.date)
            out[j,4]<-as.character(unique(POLYGON))
          }

          if(length(unique(POLYGON))>1){
            out[j,2]<-as.character(target.date)
            out[j,3]<-as.character(closest.date)
            out[j,4]<-'multiple fas'
          }
        }

        if(length(closest.date)==0){
          out[j,2]<-as.character(target.date)
          out[j,3]<-NA
          out[j,4]<-'no fa in range'
        }

        colnames(out)<-c('cfv','date.missing.fa','closest.date.with.fa','closest.date.fa')
      }
      my.list[[i]]<-out
    }
    results<-as.data.frame(do.call("rbind", my.list))
    results$key.date<-paste0(results$cfv,"-", gsub("-","",results$date.missing.fa))
    df.unassigned$key.date<-paste0(df.unassigned$cfv,"-", gsub("-","",df.unassigned$dateland))
    results2<-results[-which(is.na(results$closest.date.with.fa)|results$closest.date.fa=='multiple fas'|results$closest.date.fa=='no fa in range'),]
    results3<-distinct_all(results2[,4:5])

    to.find<-results3$key.date
    to.replace<-results3$closest.date.fa
    to.search<-df.unassigned$key.date
    x<-eclectic::find_replace(to.search,to.find,to.replace,"NA")

    df.unassigned$fleet<-x[,2]
    df.unassigned<-df.unassigned[,-which(names(df.unassigned)=='key.date')]
    filled<-rbind(df.unassigned,df.assigned)
    }



  # ////////////////////////////////////////////
  # Within plus or minus N days of PREVIOUS year ----
  # ////////////////////////////////////////////
  if(match.type=='previous.year'){
    N<-num.days
    index<-which(unique(df.unassigned[,'cfv'])%in%df.assigned[,'cfv'])
    check.boats<-unique(df.unassigned[,'cfv'])[index]
    index<-check.boats

    my.list<-list()
    for(i in 1:length(index)){
      boat<-index[i]
      out<-matrix(nrow=length(which(df.unassigned$cfv%in%index[i])),ncol=4)

      out[,1]<-boat
      cgeo<-df.assigned[which(df.assigned$cfv%in%boat),]
      cnogeo<-df.unassigned[which(df.unassigned$cfv%in%boat),]

      for(j in 1:nrow(cnogeo)){# loop each row; might be slow

        row<-cnogeo[j,]
        target.date<-as.Date(row$dateland,"%Y%m%d")

        prior.year <- as.POSIXlt(as.Date(row$dateland,"%Y%m%d"))
        prior.year$year <- prior.year$year-1
        prior.year <- as.Date(prior.year)

        #create date range limits
        #start_date <- target.date-N
        #end_date <- target.date+N
        search.date<-prior.year
        start_date <- search.date-N
        end_date <- search.date+N

        compare.dates<-as.Date(cgeo$dateland,"%Y%m%d")
        sort(compare.dates)
        compare.dates<-compare.dates[which(compare.dates>=start_date&compare.dates<=end_date)]#within range
        closest.date<-compare.dates[which.min(abs(compare.dates - target.date))]#closest date

        if(length(closest.date)>0){
          POLYGON<-cgeo[which(cgeo$cfv%in%boat&as.Date(cgeo$dateland,"%Y%m%d")%in%closest.date),'fleet']

          if(length(unique(POLYGON))==1){
            out[j,2]<-as.character(target.date)
            out[j,3]<-as.character(closest.date)
            out[j,4]<-as.character(unique(POLYGON))
          }

          if(length(unique(POLYGON))>1){
            out[j,2]<-as.character(target.date)
            out[j,3]<-as.character(closest.date)
            out[j,4]<-'multiple fas'
          }
        }

        if(length(closest.date)==0){
          out[j,2]<-as.character(target.date)
          out[j,3]<-NA
          out[j,4]<-'no fa in range'
        }

        colnames(out)<-c('cfv','date.missing.fa','closest.date.with.fa','closest.date.fa')
      }
      my.list[[i]]<-out
    }
    results<-as.data.frame(do.call("rbind", my.list))

    results$key.date<-paste0(results$cfv,"-", gsub("-","",results$date.missing.fa))
    df.unassigned$key.date<-paste0(df.unassigned$cfv,"-", gsub("-","",df.unassigned$dateland))
    results2<-results[-which(is.na(results$closest.date.with.fa)|results$closest.date.fa=='multiple fas'|results$closest.date.fa=='no fa in range'),]
    results3<-distinct_all(results2[,4:5])

    to.find<-results3$key.date
    to.replace<-results3$closest.date.fa
    to.search<-df.unassigned$key.date
    x<-eclectic::find_replace(to.search,to.find,to.replace,"NA")
    df.unassigned$fleet<-x[,2]
    df.unassigned<-df.unassigned[,-which(names(df.unassigned)=='key.date')]
    filled<-rbind(df.unassigned,df.assigned)
    }

  #//////////////////////////////////////////////////////////----

  return(filled[,-which(names(filled)=='key')])
  }
















