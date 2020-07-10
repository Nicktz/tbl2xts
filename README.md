tbl2xts
=======

![](https://cranlogs.r-pkg.org/badges/tbl2xts?color=brightgreen)
![](https://cranlogs.r-pkg.org/badges/grand-total/tbl2xts?color=brightgreen)

Introduction
------------

This package helps users who want to put data.frame or tbl\_df objects
into xts format easily. Xts is a powerful package used to convert data
frames into time-series. This package is widely used in other packages
in R too.

The problem is that often times users want to move from a tbl format, or
a tidyverse format, into xts, but doing so can be an onerous task. This
package aims to overcome this problem by bridging the tbl\_df or
data.table to xts gap. It also allows the user to use a spread\_by
argument for an easy character column xts conversion for tidy data.

Examples
========

To illustrate the ease with which tbl2xts transforms data.frames or
tbl\_dfs into xts format, consider the following example.

We use a simple toy dataset to illustrate. As we are transforming

Example 1
---------

Transform a dataframe into xts format:

    library(dplyr)
    library(tbl2xts)
    xtsdata <- 
    tbl2xts::TRI %>% 
    tbl_xts(., cols_to_xts = TRI, spread_by = Country)

Notice as of version 1.0.0 of this package, inputs are now quo\_sures \~
so you don’t have to use quotations for parameters.

We could also add a suffix or prefix to the column name created. In the
example below, adding spread\_name\_pos = “Suffix” makes columns
<Country>\_TRI.

    xtsdata <- tbl2xts::TRI %>% tbl_xts(., cols_to_xts = TRI, spread_by = Country, spread_name_pos = "Suffix")

You could also easily transform multiple numeric columns into xts using
the spread\_by functionality:

    spread_multiple <- tbl2xts::TRI %>% tbl_xts(., cols_to_xts = c("TRI", "Return"), spread_by = Country)

Notice for this example, I forced a spread\_name\_pos = “Suffix” to
allow for differentiation as you chose to spread\_by and have multiple
cols\_to\_xts.

### To now transform the xts object back into a tbl\_df() object, simply use the inverse command:

    xtsdata <- tbl2xts::TRI %>% tbl_xts(., cols_to_xts = TRI, spread_by = Country, spread_name_pos = "Suffix")
    tbl <- xtsdata %>% xts_tbl()

Example 2
---------

tbl\_xts also facilitates the use of tbl\_df data frames in packages
that use xts. As an illustration, see the output for TRI with the
package PerformanceAnalytics. As we are working with Total Return Index
values, suppose we wanted to calculate the weekly returns, and then the
… using PerformanceAnalytics. This can now be achieved with neat code as
follows:

    library(dplyr)
    library(tbl2xts)
    library(PerformanceAnalytics)
    tbldata <- tbl2xts::TRI
    tbldata %>% tbl_xts(., cols_to_xts = TRI, spread_by = Country) %>% 
    lapply(.,Return.calculate, "discrete") %>% Reduce(merge,.) %>% table.DownsideRisk(.)

Note that lapply was used to apply the Return.Calculate to each column,
while Reduce was used to merge all the lists created using lapply into a
single data frame. We then pipe all the returns into, e.g., the
table.DownsideRisk function, or any other applicable function in the PA
package.

Example 3:
----------

Here I show how you can easily do quite advanced calculations and
seemlessly move from tbl to xts back to tbl. Below I use xts’
apply.yearly and PerformanceAnalytics’ suite of calcs that allow us to
do some nice calculations easily:

    tbl2xts::TRI %>% tbl_xts(., spread_by = Country) %>%  
    PerformanceAnalytics::Return.calculate(.) %>% 
    xts::apply.yearly(., FUN = PerformanceAnalytics::StdDev.annualized) %>% 
    xts_tbl

Note how easy we could do quite advanced calculations in a single pipe.
E.g. if tasked with producing a plot for the annual CVaR numbers for our
countries:

    library(ggplot2)
    library(tidyr)
    TRI %>% tbl_xts(., spread_by = Country) %>%  
    PerformanceAnalytics::Return.calculate(.) %>% 
    xts::apply.yearly(., FUN = PerformanceAnalytics::CVaR) %>% 
    xts_tbl %>% tidyr::gather(Country, CVaR, -date) %>% 
    ggplot() + geom_line(aes(date, CVaR)) + theme_bw() + facet_wrap(~Country)

Note
----

The input requires the data to be in data.frame() or tbl\_df() form,
with a valid date column being provided (similar to the requirement for
the xts package). The function will check if a column date, DATE, or
Date exists, or alternatively look for a valid timeBased object in your
worfkile and rename it Date. This column can be either a Date, POSIXct,
chron, yearmon, yearqtr or timeDate (see ?timeBased) column. The
validity of the date column can be tested using:

    data(TRI)
    TRI[,"Date"] %>% .[[1]] %>% timeBased()

If TRUE, it is a valid date column.
