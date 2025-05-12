#' Fill NAs (replace NAs) in a column based on group summaries.
#'
#' @param df A data frame
#' @param group.cols Names of columns to group by.
#' @param update.col Names of column that should be summarized.
#' @param fun The function for summarizing (e.g., mean, max etc.)
#' @return A data frame
#' @import dplyr
#' @examples
#' #df<-fs_get_data(years=2016:2017,species.sought=144,gclass=1,gearcode=41,nafo=c('4t','4s'))
#' #test<-fs_fill_col(df,group.cols=c('year','nafodiv'),update.col='nugear',fun=mean)
#' #summary(df$nugear)
#' #summary(test$nugear)
#' #test.fail<-fs_fill_col(df,group.cols=c('year','nafodiv'),update.col=c('nugear','amtgear'),fun=mean)
#' @export
fs_fill_col<-function(df,group.cols,update.col,fun){

  if(length(update.col)>1){stop('\r Can only update one column at a time',call. = FALSE)}

  df2<-df|>
    dplyr::group_by(dplyr::across(any_of(group.cols)))|>
    dplyr::summarise(dplyr::across(any_of(update.col), \(x) .funs=fun(x, na.rm = TRUE)))
  names(df2)[ncol(df2)]<-'val'
  df2[,ncol(df2)]<-round(df2[,ncol(df2)])
  tmp<-dplyr::left_join(df,df2)
  tmp[,update.col]<-ifelse(is.na(tmp[,update.col]),tmp$val,tmp[,update.col])
  return(tmp[,-ncol(tmp)])
}
