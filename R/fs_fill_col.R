#' Fill NAs (replace NAs) in a column based on group summaries.
#'
#' @param df A data frame
#' @param group.cols Names of columns to group by.
#' @param update.col Names of column that should be summarized.
#' @param fun The function for summarizing (e.g., mean, max etc.)
#' @return A data frame
#' @import dplyr
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function was deprecated. Please use `fs_fill_missing` instead.
#' @keywords internal
#' @export
fs_fill_col<-function(df,group.cols,update.col,fun){

  stop('This function was deprecated. Please use `fs_fill_missing` instead.', call. = FALSE)


  if(length(update.col)>1){stop('\r Can only update one column at a time',call. = FALSE)}

  df2<-df|>
    dplyr::group_by(dplyr::across(any_of(group.cols)))|>
    dplyr::summarise(dplyr::across(any_of(update.col), \(x) .funs=fun(x, na.rm = TRUE)))
  names(df2)[ncol(df2)]<-'val'
  df2[,ncol(df2)]<-round(df2[,ncol(df2)])
  tmp<-dplyr::left_join(df,df2)
  tmp[which(is.na(tmp[,update.col])),update.col]<-tmp[which(is.na(tmp[,update.col])),'val']
  return(tmp[,-ncol(tmp)])
}
