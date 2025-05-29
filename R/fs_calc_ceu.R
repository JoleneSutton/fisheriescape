#' Calculate average CEU
#'
#' Uses set cards and length cards.
#' @param pivot4 Object returned from function `fs_pivot4`
#' @import dplyr
#' @importFrom utils read.csv
#' @returns A data frame
#' @export
fs_calc_ceu<-function(pivot4=NULL){

  #////////////////////////////////////////////////////////////////////////----
  # Set up ----
  #////////////////////////////////////////////////////////////////////////
  prompt.code<-c(1,2,3,4,5,6,7,8,9)
  prompt.fishery<-c('American lobster, NAFO 4S','American lobster, NAFO 4T','Atlantic cod','Atlantic herring','Atlantic halibut','Atlantic mackerel','Greenland halibut','rock crab', 'snow crab', 'toad crab','whelk')

  user.input<-function(choose.fishery){
    prompt<-utils::menu(c(prompt.fishery))
    return(prompt)
  }

  fishery<-user.input(choose.fishery)
  fishery<-prompt.fishery[fishery]

  message(paste0("Calculating average CEU for ", fishery,'.'))

  if(fishery%in%c('Atlantic herring')) {message("Atlantic herring CEU calculations are specific to NAFO 4T.")}

  #////////////////////////////////////////////////////////////////////////----
  # Fishery specific CEU calculations ----
  #////////////////////////////////////////////////////////////////////////
  if(!is.null(pivot4)){

    if(fishery%in%c('American lobster, NAFO 4S')) {
      pivot4$num.lines<-pivot4$total.gear
      pivot4$soak.time<-24
      pivot4$rope.soak.time<-pivot4$soak.time/24
      pivot4$magnitude<-pivot4$num.lines
      pivot4$intensity<-pivot4$magnitude*pivot4$rope.soak.time
      pivot4$ceu = pivot4$intensity*pivot4$prop.week.fished

      ceu2<-pivot4|>
        group_by(fleet,year,sw)|>
        summarise(ceu = sum(ceu),
                max.sum.av.traps=max(total.gear))

      (base<-length(unique(ceu2$year)))
      av.ceu<-ceu2|>
        group_by(sw,fleet)|>
        summarise(av.ceu = mean(ceu))
    }


    if(fishery%in%c('American lobster, NAFO 4T')) {

    }






    if(fishery%in%c('Atlantic cod','Atlantic halibut','Atlantic mackerel','Greenland halibut')) {
      pivot4$magnitude<-pivot4$total.gear*2
      pivot4$intensity<-pivot4$magnitude*(pivot4$soak.time/24)
      pivot4$ceu<-pivot4$intensity*pivot4$prop.week.fished

      (base<-length(unique(pivot4$year)))
      av.ceu<-pivot4|>
        dplyr::group_by(fleet,sw)|>
        dplyr::summarise(av.ceu = sum(ceu)/base)
    }




  if(fishery%in%c('rock crab','snow crab','toad crab','whelk')) {
    pivot4$rope.soak.time<-pivot4$soak.time/24
    pivot4$magnitude<-pivot4$total.gear
    pivot4$intensity<-pivot4$magnitude*pivot4$rope.soak.time
    pivot4$ceu = pivot4$intensity*pivot4$prop.week.fished

    (base<-length(unique(pivot4$year)))
    av.ceu<-pivot4|>
      dplyr::group_by(fleet,sw)|>
      dplyr::summarise(av.ceu = sum(ceu)/base)
    }




    if(fishery%in%c('Atlantic herring')) {
      pivot4$one.rope<-0
      ceu<-pivot4
      ceu4t<-utils::read.csv('analysis/herring/2025/data/ceu_4T.csv',header=TRUE)
      names(ceu4t)<-tolower(names(ceu4t))
      names(ceu4t)[3]<-'fleet'

      in.stp<-(ceu4t$num.days.fished/7)*ceu4t$mean.days.fished
      in.stp<-ifelse(in.stp>1,1,in.stp)
      ceu4t$prop.week.fished<-in.stp

      ceu$sw<-as.numeric(ceu$sw)
      CEU<-bind_rows(ceu,ceu4t)
      CEU<-CEU[,1:16]
      CEU$rope.soak.time<-CEU$soak.time/24
      CEU$magnitude<-((CEU$total.gear*CEU$one.rope*1) + (CEU$total.gear*(1-CEU$one.rope)*2))
      CEU$intensity<-CEU$magnitude*CEU$rope.soak.time
      CEU$ceu<-CEU$prop.week.fished*CEU$intensity

      (base<-length(unique(CEU$year)))
      av.ceu <- CEU %>%
        group_by(fleet, sw) %>%
        summarise(av.ceu=sum(ceu,na.rm=T)/base)
    }

    #////////////////////////////////////////////////////////////////////////----
    # Return av.ceu----
    #////////////////////////////////////////////////////////////////////////
    return(av.ceu)
  }}
