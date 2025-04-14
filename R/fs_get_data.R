#' Get and process ziff data for fisheriescape project.
#'
#' @param years Which years?
#' @param species.sought Which species sought statac code?
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

  ziff<-get.gulf::get_ziff(years = years, species.sought = species.sought)
  ziff<-ziff[which(ziff$gclass==gclass),]
  ziff<-ziff[which(ziff$gearcode%in%c(gearcode)),]
  names(ziff)<-gsub('.dd',"",names(ziff))

  if(nrow(ziff)==0){stop('\r No matches found',call. = FALSE)}


  index<-eclectic::grep_any(nafo,ziff$nafodiv)
  if(length(index)==0){stop('\r No matches found',call. = FALSE)}


  ziff<-ziff[index,]
  ziff$cfv<-gsub("[[:space:]]", "", ziff$cfv) #strip all whitespace

  #///////////////////////////////////////////////////////////////////////
  # Correct dates ----
  #///////////////////////////////////////////////////////////////////////

  #//////////////////////////////////////////////////////////
  ## if  missing dateland ----
  # in cases where dateland was "000000" or empty, the ctchdate was used as the dateland
  index<-which(startsWith(as.character(ziff$dateland),'0')|startsWith(as.character(ziff$dateland)," "))
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
  index<-which(startsWith(as.character(ziff$ctchdate),'0')|startsWith(as.character(ziff$ctchdate)," "))
  if(length(index)>0){ziff[index,"ctchdate"]<-ziff[index,"dateland"]}
  #//////////////////////////////////////////////////////////////////////////
  #//////////////////////////////////////////////////////////////////////////
  #//////////////////////////////////////////////////////////////////////////
  #//////////////////////////////////////////////////////////////////////////



  iso<-ISOweek::ISOweek(ziff$dateland)
  iso<-base::substring(iso,7,8)
  ziff$sw<-iso

  ziff$year<-as.numeric(substring(ziff$dateland,1,4))


  event.id<-paste(ziff$cfv,ziff$dateland,ziff$ctchdate,ziff$latitude,ziff$longitude,sep=';')
  trip.id<-paste(ziff$cfv,ziff$dateland,ziff$ctchdate,sep=';')

  ziff<-cbind(trip.id,event.id, ziff)


  ziff<-dplyr::distinct_all(ziff)

  var<-c("trip.id","event.id","cfv","dateland","nafodiv",'unitarea',"mangare",
         "gearcode","gclass","daysea","daysgr","daysfish","hourfish","nugear",
         "ctchdate","depth","depthcode","region",
         "licclas","licence" ,"fisharea","seqnum","year","fishery","amtgear",
         "longitude","latitude",'sw')

  ziff<-ziff[,var]
  ziff[ziff == ""] <- NA

  ziff[which(ziff$hourfish==0),'hourfish']<-NA
  ziff[which(ziff$nugear==0),'nugear']<-NA
  ziff[which(ziff$amtgear==0),'amtgear']<-NA

  ziff$fisharea<-tolower(ziff$fisharea)
  ziff$unitarea<-tolower(ziff$unitarea)
  ziff$mangare<-tolower(ziff$mangare)
  ziff$fishery<-tolower(ziff$fishery)


  index<-which(!is.na(ziff$latitude)|!is.na(ziff$longitude))
  if(length(index)>0){
  ziff[index,'depth.gebco']<-gslSpatial::get_depth(ziff[index,'longitude'],ziff[index,'latitude'])

  pts<-terra::vect(ziff[index,],geom=c('longitude','latitude'), crs='epsg:4269')
  pts<-terra::project(pts,'ESRI:102001')
  pts<-terra::geom(pts)
  ziff$x<-NA
  ziff$y<-NA
  ziff[-index,'x']<-pts[,3]
  ziff[-index,'y']<-pts[,4]
  }

  return(ziff)
}

























