#' Get and process ziff data for fisheriescape project.
#'
#' @param years Which years? Required
#' @param species.sought Which species sought statac code? Required
#' @param nafo Which NAFO divisions? Case insensitive
#' @param gclass Which gear class?
#' @param gearcode Which gear code?
#' @import gulf
#' @import get.gulf
#' @importFrom gslSpatial get_depth
#' @importFrom eclectic grep_any
#' @importFrom ISOweek ISOweek
#' @importFrom dplyr distinct_all
#' @importFrom terra vect geom project
#' @examples
#' #fs_get_data(years = 2017, species.sought = 705, nafo=c('4r'), gclass='1', gearcode=c('62','99'))
#' @export
fs_get_data<-function(years=NULL,
                      species.sought=NULL,
                      nafo=NULL,
                      gclass=NULL,
                      gearcode=NULL){

  if(is.null(years)){stop('\r Must specify years',call. = FALSE)}
  if(is.null(species.sought)){stop('\r Must specify species.sought',call. = FALSE)}


  ziff<-get.gulf::get_ziff(years = years, species.sought = species.sought)
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

  #//////////////////////////////////////////////////////////
  ## if ctchdate are after dateland ----
  #//////////////////////////////////////////////////////////
  ##### if the date landed is BEFORE date caught, change date caught to date landed
  index<-which(ziff$dateland < ziff$ctchdate)
  if(length(index)>0){ziff[index,"ctchdate"]<-ziff[index,"dateland"]}


  #//////////////////////////////////////////////////////////
  ## if missing date.caught ----
  #//////////////////////////////////////////////////////////
  # in cases where dateland was "000000" or empty, the ctchdate was used as the dateland
  index<-which(startsWith(as.character(ziff$ctchdate),'0')|startsWith(as.character(ziff$ctchdate)," ")|is.na(ziff$ctchdate))
  if(length(index)>0){ziff[index,"ctchdate"]<-ziff[index,"dateland"]}
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

























