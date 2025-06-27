#' Assign missing fleets based on geographic coordinates.
#'
#' @param df A data frame
#' @param polygon A shapefile (class: SpatVector, geometry:polygons) with column "fleet"
#' @importFrom gslSpatial assign_points_terra assign_points_to_nearest_polygon
#' @importFrom terra project crs geomtype

#' @export
fs_fleet_by_coordinates<-function(df, polygon){

  # checks
  if(grep('fleet',names(df))<1){df$fleet<-NA}
  if(grep("SpatVector", class(polygon))<1){stop('\r polygon must be class "SpatVector."',call. = FALSE)}
  if(grep("polygons",terra::geomtype(polygon))<1){stop('\r polygon must be geomtype "polygons."',call. = FALSE)}
  if(grep('fleet',names(polygon))<1){stop('\r polygon must include a "fleet" column.',call. = FALSE)}

  if(terra::crs(polygon, proj=FALSE, describe=TRUE, parse=FALSE)[3]!='102001'){
    polygon <- terra::project(polygon,'ESRI:102001')
    message("Projecting polygon to 'ESRI:102001.")
  }

  index<-which(!is.na(df$x)&!is.na(df$y))
  if(length(index)==0){stop('\r There are no geographic coordinates in columns "x" and "y" in df.',call. = FALSE)}
  rm(index)

  #////////////////////////////////////////////////
  # Assign ziff to geographic fleet using exact location ----
  index<-which(!is.na(df$x)&!is.na(df$y)&is.na(df$fleet))

  if(length(index)>0){
  x<-gslSpatial::assign_points_terra(df[index,'x'],df[index,'y'],polygon[,'fleet'])
  df[index,'fleet']<-x[,3]
  rm(index)
  rm(x)
  }

  #////////////////////////////////////////////////
  # Assign ziff to geographic fleet using nearest location ----
  index<-which(!is.na(df$x)&!is.na(df$y)&is.na(df$fleet))
  if(length(index)>0){
    x<-gslSpatial::assign_points_to_nearest_polygon(df[index,'x'],df[index,'y'],polygon[,'fleet'],'fleet')
    df[index,'fleet']<-x[,3]
    rm(index)
    rm(x)
  }

}
