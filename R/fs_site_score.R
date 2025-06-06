#' Fisheriescape calculate site score
#'
#' @param df The for.site.score data frame.
#' @param fa.poly The fishing area polygon. CRS should be albers. Must contain a column called 'fleet'.
#' @param grid Grid shapefile. If NULL, defaults to 'dfo_hex_coffen_smout_cropped.shp'.
#' @import dplyr
#' @importFrom terra vect crs crop merge
#' @importFrom gslSpatial get_shapefile assign_points_terra
#' @importFrom sf st_as_sf st_intersection st_area
#' @export
fs_site_score<-function(df,fa.poly,grid=NULL){

  # appease R CMD check
  GRID_ID=NULL
  fleet=NULL
  area.in.fa=NULL
  year=NULL
  sw=NULL
  tot.count.hex=NULL
  av.count.hex=NULL
  sum.av.count.fleet=NULL
  area=NULL


  #if(is.null(grid)){grid<-terra::vect('inst/extdata/dfo_hex_coffen_smout_cropped.shp')} #move this to gslSpatial!
  if(is.null(grid)){grid<-gslSpatial::get_shapefile('hex')}

  #//////////////////////////////////////////////////////////////////////////
  # Crop grid to extent of data points ----
  pts<-terra::vect(df,geom=c('x','y'),crs=terra::crs(grid))
  hex<-terra::crop(grid,terra::ext(pts)*1.25)

  #//////////////////////////////////////////////////////////////////////////
  # Assign points to hex ----
  x<-gslSpatial::assign_points_terra(df$x,df$y,hex[,'GRID_ID'])
  df$GRID_ID<-x[,3]

  if(length(which(is.na(df$GRID_ID)))>0){stop('\r Not all data points were assigned to a hexagon',call. = FALSE)}

  #//////////////////////////////////////////////////////////////////////////
  # Assign hexes to fleet based on amount of overlap----
  #https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r

  hex2<-hex
  x<-sf::st_as_sf(hex2)
  y<-sf::st_as_sf(fa.poly)
  pi <- sf::st_intersection(y, x) #

  # add in areas in m2
  #attArea <- pi |>
   # dplyr::mutate(area = sf::st_area(.) |>
    #                as.numeric())
  attArea <- pi |>
    dplyr::mutate(area = sf::st_area(pi) |>
                    as.numeric())

  # for each GRID_ID, get area per fleet
  attArea<-attArea |>
    dplyr::as_tibble() |> # or tidyr?
    dplyr::group_by(GRID_ID, fleet) |>
    dplyr::summarize(area.in.fa = sum(area))
  #attArea #hexes that overlap multiple fishing areas are repeated, with area in each fishing area shown

  attArea<-attArea |> # which fishing area contains most of each hex?
    dplyr::group_by(GRID_ID) |>
    dplyr::filter(area.in.fa == max(area.in.fa))


  hex3<-terra::merge(terra::vect(x),attArea)

  #ggplot()+
  #  geom_spatvector(data=fa.poly,fill='white')+
  #  geom_spatvector(data=hex3,aes(fill=fleet))+
  #  geom_spatvector(data=fa.poly,fill=NA)+
  #  coord_sf(crs=crs(fa.poly),datum = crs(fa.poly))

  hex3$fleet.hex.area<-hex3$fleet
  #//////////////////////////////////////////////////////////////////////////
  # Compare ----

  #head(df)
  df2<-dplyr::left_join(df,as.data.frame(hex3[,c('GRID_ID',"fleet.hex.area")]))
  #nrow(df)
  #nrow(df2)
  #head(df2)
  #length(which(is.na(df2$fleet)))
  #length(which(is.na(df2$fleet.geo)))
  #length(which(is.na(df2$fleet.hex.area)))

  #/////////////////////////////////////////////////////////////////////////
  # Count points per hex per year per week ----
  counts<-df2|>
    dplyr::group_by(year,sw,GRID_ID)|>
    dplyr::summarise(count = dplyr::n())


  #/////////////////////////////////////////////////////////////////////////
  # Mean count per hex per week ----
  (base<-length(unique(counts$year)))
  mean.count <- counts |>
    dplyr::group_by(GRID_ID, sw) |>
    dplyr::summarise(tot.count.hex=sum(count,na.rm=T),
              av.count.hex = tot.count.hex/base)



  #/////////////////////////////////////////////////////////////////////////
  # Standardize----
  # for each fishing area, av.count.hex/sum(av.count.hex)

  tmp<-dplyr::distinct(df2[,c('GRID_ID','fleet.hex.area')])
  #summary(as.numeric(table(tmp$GRID_ID))) # if all one, then each hex assigned to single fleet

  tmp2<-dplyr::left_join(mean.count,tmp)
  names(tmp2)[which(names(tmp2)=='fleet.hex.area')]<-'fleet'

  site.score<-tmp2|>
    dplyr::group_by(fleet,sw)|>
    dplyr::mutate(sum.av.count.fleet= sum(av.count.hex),
           prop.count.fleet = av.count.hex/sum.av.count.fleet)

  if(length(which(is.na(site.score$fleet)))>0){
    index<-which(is.na(site.score$fleet))
    site.score[index,c('sum.av.count.fleet', 'prop.count.fleet')]<-NA
  }

  return(site.score)

}
