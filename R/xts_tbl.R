#' @title xts_tbl
#' @description This function converts data from a xts object to a tbl_df().
#' Note that the dataframe must be of type xts and ordered by a date column. This date column will be preserved and save as "date".
#' @param xts A xts series that will be converted to a tbl_df().
#' @param Colnames_Exact Stops xts natively replacing spaces in column names with full stops. Kept FALSE as default, as most users expect this behavior.
#' @return A tbl_df() with the first column the "date"  column used to order the xts series by.
#' @importFrom xts as.xts
#' @import zoo
#' @import dplyr
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' library(dplyr)
#' data(TRI)
#' df_xts_tbl <- TRI %>% tbl_xts(., cols_to_xts = TRI, spread_by = Country) %>% xts_tbl()
#' }
#' @export

xts_tbl <- function(xts, Colnames_Exact = FALSE) {

  # Sanity Check -----------------------------------------------------------
  # ensure that column 1 is a valid date column:
  if ( class(xts)[1] !=  "xts") stop("Ensure that the supplied dataframe has class xts...")

  Names <- colnames(coredata(xts))
  df <- data.frame(date=index(xts), coredata(xts)) %>% tibble::as_tibble()

  if(Colnames_Exact) {colnames(df) <- c("date", Names)}


  df

}

