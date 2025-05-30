#' Fill NAs (replace NAs) in duplicate rows. Use with caution (see examples)!!
#'
#' @param df A data frame
#' @param group.cols Names of columns to group by.
#' @param fill.cols Names of columns that should be filled.
#' @return A data frame
#' @import dplyr
#' @importFrom tidyr fill
#' @examples
#' Lines <- "ID Value1 Value2 Value3 Value4 Value5 Value6
#' 1 A B C z z NA
#' 1 A B C y NA z
#' 2 A B C NA x NA
#' 2 A B C x NA NA
#' 3 A B C x NA NA
#' 3 A B C x y z"
#' (DF <- read.table(text = Lines, header = TRUE, as.is = TRUE))
#' fs_fill_rows(DF,1,2:ncol(DF))# works. Note columns "Value5" and "Value6" for ID "1"
#' fs_fill_rows(DF,"ID",2:ncol(DF))# works.  Note columns "Value5" and "Value6" for ID "1"
#' fs_fill_rows(DF,c(1,5),2:ncol(DF)) #works. Compare to previous examples.
#' fs_fill_rows(DF,"ID","Value1:Value6")# does nothing
#' @export
fs_fill_rows<-function(df,group.cols,fill.cols){
  out<-df |>
    dplyr::group_by(dplyr::across(any_of(group.cols)))|>
    tidyr::fill(any_of(fill.cols), .direction = "downup")|>
    dplyr::distinct_all()
  return(out)
}
