#' Get and process ziff data for fisheriescape project.
#'
#' @param years Which years? Required
#' @param species.sought Which species sought statac code? Required
#' @param nafo Which NAFO divisions? Case insensitive
#' @param gclass Which gear class?
#' @param gearcode Which gear code?
#' @importFrom gslSpatial get_depth convert_dms_to_dd
#' @importFrom eclectic grep_any
#' @importFrom ISOweek ISOweek
#' @importFrom dplyr distinct_all
#' @importFrom terra vect geom project
#' @importFrom data.table fread
#' @examples
#' #fs_get_data(years = 2017, species.sought = 705, nafo=c('4r'), gclass='1', gearcode=c('62','99'))
#' @export
fs_get_data<-function(years=NULL,
                      species.sought=NULL,
                      nafo=NULL,
                      gclass=NULL,
                      gearcode=NULL){

  message("This function requires access to internal drives at DFO Gulf Region.")

  if(is.null(years)){stop('\r Must specify years',call. = FALSE)}
  if(is.null(species.sought)){stop('\r Must specify species.sought',call. = FALSE)}

  #///////////////////////////////////////////////////////////////
  ## remove 'get.gulf' and `gulf` packages as a dependencies by re-creating the get_ziff function here
  get_ziff<-function(years, nafo=NULL, species.caught=NULL, species.sought=NULL, headers='english'){

    options(scipen = 999)# turn off scientific notation because it interferes with converting deg/min/sec to dd


    if(.Platform$OS.type=='unix'){path<-'/mnt/AquaRes_Common/FishFramSci/ziff_QC/'}else{
      path<-'//ENT.dfo-mpo.ca/dfo-mpo/GROUP/GLF/Regional_Shares/AquaRes_Common/FishFramSci/ziff_QC/'}

    p1<-'ziff_'

    my.files<-list.files(path=path,pattern=p1)
    my.files<-my.files[grep(pattern=paste0(paste0(p1,years),collapse="|"),my.files)]

    ziff <- data.frame()

    for (i in 1:length(my.files)){
      print(paste0("Getting landings for ",  gsub(".*?([0-9]+).*", "\\1", my.files[i])   ))
      dat<-readRDS(paste(path, my.files[i], sep = ""))


      # Begin if statements///////////////////////////////////
      if(!is.null(nafo)){
        dat<-dat[c(grep(pattern=paste0(nafo,collapse="|"), dat$opano, ignore.case = T)),]
      }

      if(!is.null(species.caught)){
        dat<-dat[c(grep(pattern=paste0(species.caught,collapse="|"), dat$cod_esp, ignore.case = T)),]
      }

      if(!is.null(species.sought)){
        dat<-dat[c(grep(pattern=paste0(species.sought,collapse="|"), dat$prespvis, ignore.case = T)),]
      }

      ziff <- rbind(ziff, dat)
      ziff<-as.data.frame(ziff)
      rm(dat)
    }

    if(headers =='english'){
      new.names<-data.table::fread(paste0(path, 'qc_ziff_metadata.csv'),header = TRUE)
      index<-match(names(ziff),new.names$Nom_fr) #reorder the second to match the first
      #cbind(names(ziff),new.names$Nom_fr[index],new.names$Name_en[index])
      new.names<-new.names$Name_en[index]
      names(ziff)<-new.names
      rm(new.names)
    }
    # End if statements/////////////////////////////////////



    # //////////////////////////////////////////////////////
    # Sometimes the QC_latitude and QC_longitude coordinates are reversed
    # Correct this by using the original coordinate columns

    #ziff<-ziff[,-which(names(ziff)%in%c('Q_longitude','Q_latitude'))]# remove problem columns

    #some might be deg-min, but some might be deg-min-sec
    ziff$nchar.org.coord<-nchar(ziff$long)
    ziff$unit.org.coord<-ifelse(ziff$long>1000&ziff$nchar.org.coord==4,'deg-min',
                                ifelse(ziff$long>1000&ziff$nchar.org.coord>4,'deg-min-sec','dd'))

    #table(ziff$unit.org.coord)

    index1<-which(ziff$unit.org.coord=='deg-min-sec')
    index2<-which(ziff$unit.org.coord=='deg-min')
    index3<-which(ziff$unit.org.coord=='dd')

    ziff$longitude.dd<-NA
    ziff$latitude.dd<-NA

    suppressMessages({
      if(length(index1)>0){
        ziff[index1, 'longitude.dd']<-gslSpatial::convert_dms_to_dd(ziff[index1, 'long'])*-1
        ziff[index1, 'latitude.dd']<-gslSpatial::convert_dms_to_dd(ziff[index1, 'lat'])
      }

      if(length(index2)>0){
        ziff[index2, 'longitude.dd']<-gslSpatial::convert_dms_to_dd(ziff[index2, 'long']*100)*-1
        ziff[index2, 'latitude.dd']<-gslSpatial::convert_dms_to_dd(ziff[index2, 'lat']*100)
      }

      if(length(index3)>0){
        ziff[index3, 'longitude.dd']<-abs(ziff[index3, 'long'])*-1
        ziff[index3, 'latitude.dd']<-ziff[index3, 'lat']
      }
    })
    # //////////////////////////////////////////////////////

    #head(ziff)

    return(ziff)
  }
  #///////////////////////////////////////////////////////////////

  ziff<-get_ziff(years = years, species.sought = species.sought)
  names(ziff)<-gsub('.dd',"",names(ziff))

  #filters
  if(!is.null(gclass)){
  if(length(which(ziff$gclass%in%gclass))>0){ziff<-ziff[which(ziff$gclass%in%gclass),]}else{print("No matches to gclass; ignoring gclass")}
  }

  if(!is.null(gearcode)){
  if(length(which(ziff$gearcode%in%gearcode))>0){ziff<-ziff[which(ziff$gearcode%in%gearcode),]}else{print("No matches to gearcode; ignoring gearcode")}
  }

  if(!is.null(nafo)){
  index<-eclectic::grep_any(nafo,ziff$nafodiv)
  if(length(index)>0){ziff<-ziff[index,]}else{print("No matches to NAFO; ignoring NAFO")}
  }


  if(nrow(ziff)==0){stop('\r No matches found',call. = FALSE)}

  ziff$cfv<-gsub("[[:space:]]", "", ziff$cfv) #strip all whitespace
  ziff$licence<-gsub("[[:space:]]", "", ziff$licence) #strip all whitespace
  ziff[which(ziff$licence=='0'),'licence']<-NA
  ziff[which(ziff$homeport=='0'),'homeport']<-NA
  ziff[which(ziff$portland=='0'),'portland']<-NA
  ziff[which(ziff$typefis==""),'typefis']<-NA

  # remove sentinel ----
  index<-which(ziff$typfis=='S')
  if(length(index)>0){ziff<-ziff[-index,]}

  #///////////////////////////////////////////////////////////////////////
  # Correct dates ----
  #///////////////////////////////////////////////////////////////////////

  #//////////////////////////////////////////////////////////
  ## if  missing dateland ----
  # in cases where dateland was "000000" or empty, the ctchdate was used as the dateland
  index<-which(startsWith(as.character(ziff$dateland),'0')|startsWith(as.character(ziff$dateland)," ")|is.na(ziff$dateland))
  if(length(index)>0){ziff[index,"dateland"]<-ziff[index,"ctchdate"]}
  rm(index)

  #//////////////////////////////////////////////////////////
  ## if missing date.caught ----
  #//////////////////////////////////////////////////////////
  # in cases where dateland was "000000" or empty, the ctchdate was used as the dateland
  index<-which(startsWith(as.character(ziff$ctchdate),'0')|startsWith(as.character(ziff$ctchdate)," ")|is.na(ziff$ctchdate))
  if(length(index)>0){ziff[index,"ctchdate"]<-ziff[index,"dateland"]}
  rm(index)

  #//////////////////////////////////////////////////////////
  ## if ctchdate are after dateland ----
  #//////////////////////////////////////////////////////////
  ##### if the date landed is BEFORE date caught, change date caught to date landed
  index<-which(ziff$dateland < ziff$ctchdate)
  if(length(index)>0){ziff[index,"ctchdate"]<-ziff[index,"dateland"]}
  rm(index)
  #//////////////////////////////////////////////////////////////////////////
  #//////////////////////////////////////////////////////////////////////////
  #//////////////////////////////////////////////////////////////////////////
  #//////////////////////////////////////////////////////////////////////////

  iso<-ISOweek::ISOweek(ziff$dateland)
  iso<-base::substring(iso,7,8)
  ziff$sw<-iso

  ziff$year<-as.numeric(substring(ziff$dateland,1,4))

  ziff[ziff == ""] <- NA
  ziff[which(ziff$daysfish=='0'),'daysfish']<-NA
  ziff[which(ziff$hourfish==0),'hourfish']<-NA
  ziff[which(ziff$nugear==0),'nugear']<-NA
  ziff[which(ziff$amtgear==0),'amtgear']<-NA

  ziff$fisharea<-toupper(ziff$fisharea)
  ziff$unitarea<-toupper(ziff$unitarea)
  ziff$mangare<-toupper(ziff$mangare)
  ziff$fishery<-toupper(ziff$fishery)

  index<-which(ziff$cfv=='0')
  if(length(index)>0){ziff<-ziff[-index,]}

  trip.id<-paste(ziff$cfv,ziff$dateland,ziff$ctchdate,sep=';')
  ziff<-cbind(trip.id,ziff)

  ziff<-ziff[order(ziff$trip.id,ziff$tripno),]

  #///////////////////////////////////////////////////////////////////////
  ## Restrict columns----
  #///////////////////////////////////////////////////////////////////////
  var<-c('trip.id',"cfv","dateland","nafodiv",'unitarea',"mangare",'grid',
         "gearcode","gclass","daysea","daysgr","daysfish","hourfish","nugear",
         "ctchdate","depth","depthcode","region",'portland',"homeport",
         "licclas","licence" ,"fisharea","seqnum","year","fishery","amtgear",
         "longitude","latitude",'sw')

  ziff2<-ziff[,var]

  #///////////////////////////////////////////////////////////////////////
  ## Remove full row duplicates ----
  ziff2<-dplyr::distinct_all(ziff2)
  #///////////////////////////////////////////////////////////////////////
  #message('Filling in missing rows. Please be patient.')
  #compress <- function(x) c(na.omit(x), NA)[1]
  #ziff<-aggregate(ziff[2:ncol(ziff)], ziff[1], compress) #if there are differences other than NA, this retains first option only
  #ziff3<- ziff2 |>
    #dplyr::group_by(trip.id) |>
    #tidyr::fill(2:ncol(ziff2), .direction = "downup")
  #ziff3<-dplyr::distinct_all(ziff3)
  #nrow(ziff2)

  ziff<-ziff2
  rm(ziff2)
  event.id<-paste(ziff$cfv,ziff$dateland,ziff$ctchdate,ziff$latitude,ziff$longitude,sep=';')
  ziff<-cbind(event.id, ziff)

  index<-which(!is.na(ziff$latitude)&!is.na(ziff$longitude))
  if(length(index)>0){
  ziff[index,'depth.gebco']<-gslSpatial::get_depth(ziff[index,'longitude'],ziff[index,'latitude'])

  pts<-terra::vect(ziff[index,],geom=c('longitude','latitude'), crs='epsg:4269')
  pts<-terra::project(pts,'ESRI:102001')
  pts<-terra::geom(pts)
  ziff$x<-NA
  ziff$y<-NA
  ziff[index,'x']<-pts[,3]
  ziff[index,'y']<-pts[,4]
  }

  return(ziff)
}

























