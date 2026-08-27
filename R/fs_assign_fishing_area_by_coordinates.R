#' A function to help associate each record to a fishing area.
#'
#' @param df A data frame with columns "x" and "y" containing coordinates.
#' @param crs The coordinate reference system.
#' @param polygon A shapefile (class: SpatVector, geometry:polygons). The first column should be fishing area ids.
#' @param nearest For records with coordinates that do not overlap with polygons, do you want to return
#' the name of the nearest polygon? Logical. Default is TRUE.
#' @importFrom gslSpatial assign_points_terra assign_points_to_nearest_polygon
#' @importFrom terra project crs geomtype
#' @export
fs_assign_fishing_area_by_coordinates<-function(df, crs, polygon, nearest=TRUE){

  # checks
  if(length(grep("SpatVector", class(polygon)))<1){stop('\r polygon must be class "SpatVector."',call. = FALSE)}
  if(length(grep("polygons",terra::geomtype(polygon)))<1){stop('\r polygon must be geomtype "polygons."',call. = FALSE)}

  polygon<-terra::project(polygon,crs)

  #if(terra::crs(polygon, proj=FALSE, describe=TRUE, parse=FALSE)[3]!='102001'){
  #  polygon <- terra::project(polygon,'ESRI:102001')
  #  message("Projecting polygon to 'ESRI:102001.")
  #}

  index<-which(!is.na(df$x)&!is.na(df$y))
  if(length(index)==0){stop('\r There are no geographic coordinates in columns "x" and "y" in df.',call. = FALSE)}
  rm(index)

  #////////////////////////////////////////////////
  # Assign to geographic fishing area using exact location ----
  message("Coordinates being assigned to polygons they overlap")
  index<-which(!is.na(df$x)&!is.na(df$y))

  if(length(index)>0){
  x<-gslSpatial::assign_points_terra(df[index,'x'],df[index,'y'],polygon[,1])
  df[index,'fa.geo']<-x[,3]
  df[index,'fa.geo.coords']<-'coordinates inside fishing area'
  rm(index)
  rm(x)
  }

  #////////////////////////////////////////////////
  # Assign to geographic fishing area using nearest location ----
  if(isTRUE(nearest)){
    message("Coordinates being assigned to nearest polygons")

      index<-which(!is.na(df$x)&!is.na(df$y))
        if(length(index)>0){
        x<-gslSpatial::assign_points_to_nearest_polygon(df[index,'x'],df[index,'y'],polygon[,1])
        df[index,'fa.geo']<-x[,3]
        df[index,'fa.geo.coords']<-'nearest fishing area to coordinates'
        rm(index)
        rm(x)
      }
  }

  return(df)

}
