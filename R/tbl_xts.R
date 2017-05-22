#' @title tbl_xts
#' @description This function converts data from a tbl_df() format into a xts format.
#' Note that the dataframe must be a data.frame or tbl_df, and either the first column must be a valid date column, or there must be one column named date, Date or DATE to order by.
#' tbl_xts also allows the user to specify the columns to be transformed to xts, as well as an option for spreading by a single character or factor type column. See the example for details.
#' @param tblData A tbl_df type dataframe
#' @param cols_to_xts Specify the columns to be converted to xts format. If not provided, it will by default transform all numeric columns to xts.
#' @param spread_by A character or factor type column used to create xts series by. See example.
#' @param spread_name_pos Add the column name of the column used to spread_by as a Suffix, Prefix or None. Defaults to Suffix (puts spread_by name at end of colname, separated by an underscore).
#' @return A xts dataframe, with columns xts series ordered by the first (date) column.
#' @importFrom xts as.xts timeBased
#' @import zoo
#' @import dplyr
#' @import lazyeval
#' @export
#' @examples
#' library(dplyr)
#' data(TRI)
#' tbl_xts(TRI, cols_to_xts = "TRI", spread_by = "Country")


tbl_xts <- function(tblData, cols_to_xts, spread_by, spread_name_pos = "Suffix") {

  # Sanity Checks -----------------------------------------------------------

  # ensure that column 1 is a valid date column:
  if ( timeBased(tblData[,1][[1]]) == FALSE & timeBased(tblData[,which(names(tblData) %in% c("Date", "date", "DATE") )][[1]]) == FALSE ) stop("Ensure that the first column is a valid time-based column, or you have a valid time based column called date, Date or DATE. \n Current time-based objects supported are Date, POSIXct, chron, yearmon, yearqtr, and timeDate")
  if ( length(tblData[,which(names(tblData) %in% c("Date", "date", "DATE") )]) > 1 ) stop("Provide only one date column named Date, date or DATE.")

  # Check classes:
  if ( !class(tblData)[1] %in% c("tbl_df", "grouped_df","data.frame") ) stop("This function can be used on dataframes or tbl_df() classes only. Check the class of your object using class(objectname)")
  if ( class(tblData)[1] != "tbl_df" & class(tblData)[1] != "grouped_df" & class(tblData)[1] != "data.frame") stop("This function can be used on dataframes or tbl_df() classes only. Check the class of your object using class(objectname)")

  # dataframe to tbl_df. Add warning too..:
  if (class(tblData)[1] == "data.frame") {
    tblData <-
      tblData %>% tbl_df()
    warning("NOTE:......... \n Changed your data.frame object to tbl_df(). \n If results are strange, use a tbl_df class and check column definitions (character, numeric, etc make sense before using tbl_xts.)")
  }

  # Warn if cols_to_xts is larger than the amount of data columns:
  if ( ncol( tblData[sapply(tblData, is.numeric)] ) == 0  ) stop("Series to Xts must be numeric.")

  # Xts Conversion ----------------------------------------------------------

  if ( missing(spread_by) ) {

    # Warn if there is a grouping to the tbl_df, but spread_by has not been provided:
    if ( class(tblData)[1] == "grouped_df" ) warning("NOTE: The tbl_df grouping was not preserved. Output same as with ungroup(df). \n You can choose to use spread_by instead.")

    # Define the date column to arrange by:
    if( length(tblData[,which(names(tblData) %in% c("Date", "date", "DATE") )]) == 0 ) {
      d <- as.Date(tblData[,1][[1]])
    } else {
      d <- as.Date(tblData[,which(names(tblData) %in% c("Date", "date", "DATE") )][[1]])
    }

    if (!missing(cols_to_xts) ) {
      if ( ncol( tblData[sapply(tblData, is.numeric)] ) < length(cols_to_xts)  ) stop("cols_to_xts input larger than available numeric columns. Check this input")
      dataXts <-
        as.xts( tblData[sapply(tblData, is.numeric)] %>% select_(.dots = cols_to_xts), order.by = d )
    } else {
      # If no cols_to_xts is provided, make all numeric columns xts...
      dataXts <-
        as.xts( tblData[sapply(tblData, is.numeric)], order.by = d )
    }

  } else {

    if ( length(spread_by) > 1 ) stop( "spread_by length greater than 1. This function only works on 1 spread column." )
    if ( spread_by[1] %in% colnames(tblData) == FALSE ) stop( cat(paste0("spread_by must be a valid column of the input object. \n ", spread_by[1], " is not a valid column.") ) )

    gid.xts <-
      as.character(unique(tblData %>% select_(spread_by))[[1]])

    if (length(gid.xts) == 1 ) message("The spread_by column only has one category. \nHence only the column name was changed...")
    if ( !class(gid.xts) %in%  c("character", "factor") ) stop( "Valid column type for spread_by requires a character or factor column")

    for (i in 1:length(gid.xts)) {

      lazyfilter <-
        lazyeval::interp(~a %in% b, a = as.name(spread_by), b = gid.xts[i])

      xtsdatTmp <-
        tblData %>% ungroup() %>% filter_(lazyfilter)

      # Define the date column to arrange by:
      if( length(xtsdatTmp[,which(names(xtsdatTmp) %in% c("Date", "date", "DATE") )]) == 0 ) {
        d <- as.Date(xtsdatTmp[,1][[1]])
      } else {
        d <- as.Date(xtsdatTmp[,which(names(xtsdatTmp) %in% c("Date", "date", "DATE") )][[1]])
      }

      if (!missing(cols_to_xts) ) {
        if ( ncol( xtsdatTmp[sapply(xtsdatTmp, is.numeric)] ) < length(cols_to_xts)  ) stop("cols_to_xts input larger than available numeric columns. Check this input")
        dataXtsTmp <-
          as.xts( xtsdatTmp[sapply(xtsdatTmp, is.numeric)] %>% select_(.dots = cols_to_xts), order.by = d )
      } else {

        # If no cols_to_xts is provided, make all numeric columns xts:
        dataXtsTmp <-
          as.xts( xtsdatTmp[sapply(xtsdatTmp, is.numeric)], order.by = d )
      }

      if(missing(spread_name_pos)) spread_name_pos <- "NONE"
      if(!spread_name_pos %in% c("Suffix", "suffix", "prefix", "Prefix", "NONE", "None", "none") ) stop("Please provide a valid spread_name_pos. \n Either: Suffix, suffix, Prefix, prefix, NONE, None or none.")

      if (spread_name_pos %in% c("Suffix", "suffix") ) {
        colnames(dataXtsTmp) <- paste(gid.xts[i],colnames(dataXtsTmp), sep = "_")
      } else
        if (spread_name_pos %in% c("prefix", "Prefix") ) {
          colnames(dataXtsTmp) <- paste(colnames(dataXtsTmp), gid.xts[i], sep = "_")
        } else
          if (spread_name_pos %in% c("NONE", "None", "none") ) {
          colnames(dataXtsTmp) <- gid.xts[i]
      # Thus: add the gid.xts as suffix, prefix, or None.
}
      if (i == 1 ) {
        dataXts <- dataXtsTmp
      } else {
        dataXts <- cbind(dataXts, dataXtsTmp)
      }

    }

  }

  dataXts

}

