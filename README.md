
# megamation <a href="https://asadow.github.io/megamation/"><img src="man/figures/logo.png" alt="megamation website" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/asadow/megamation/branch/master/graph/badge.svg)](https://app.codecov.io/gh/asadow/megamation?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

megamation provides an R interface to [Megamation
DirectLine](https://megamation.com/) via the
[API](https://apidocs.megamation.com/).

## Installation

You can install the released version of megamation from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("megamation")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("asadow/megamation")
```

## Attach megamation

``` r
library(megamation)
```

## Authorization

`mm_set_creds()` installs your API credentials:

``` r
mm_set_creds(
  key = "<YOUR-MEGAMATION_KEY>",
  url = "<YOUR-MEGAMATION_URL>"
)
```

## Get Data

`mm_get()` makes a GET request to an endpoint:

``` r
mm_get("status")
#> # A tibble: 9 × 3
#>   ampc_required description                              status
#>   <chr>         <chr>                                    <chr> 
#> 1 No            ADMIN HTML WORK ORDERS WAITING TO SCREEN AW    
#> 2 Yes           Cancelled                                CA    
#> 3 No            CLOSED                                   CL    
#> 4 No            INITIATED                                I     
#> 5 No            In Progress                              IP    
#> 6 Yes           Returned from Trade                      RT    
#> 7 No            SCHEDULED                                S     
#> 8 No            TRANSFERED                               T     
#> 9 Yes           Waiting for Trade                        WT
```

It provides informative R errors (alongside HTTP errors):

``` r
mm_get("statuses")
#> Error in `req_perform()`:
#> ! Failed to parse error body with method defined in `req_error()`.
#> Caused by error in `env_has()`:
#> ! `env` must be an environment, not `NULL`.
```

### Filter Requests

`mm_get()` allows you to easily filter requests by fields.

Let’s try to request two work orders by their work order numbers:

``` r
mm_get("workorder", wo_no = c("00001", "00002"))
#> Error in `req_perform()`:
#> ! Failed to parse error body with method defined in `req_error()`.
#> Caused by error in `env_has()`:
#> ! `env` must be an environment, not `NULL`.
```

Uh oh! `wo_no` is not available as a filterable field (yet). You can ask
your Megamation representative to add it.

#### Available Filters

`mm_get_names()` allows you to see which of your fields you can
currently filter by:

``` r
mm_get_names("workorder")
#> # A tibble: 39 × 5
#>    field           filter_enabled.x description           type  filter_enabled.y
#>    <chr>           <lgl>            <chr>                 <chr> <lgl>           
#>  1 type            TRUE             Type                  VARC… FALSE           
#>  2 status          TRUE             Status                VARC… FALSE           
#>  3 date            TRUE             Date - [Month][Day][… DATE  FALSE           
#>  4 submitted_by    TRUE             Submitted By          VARC… FALSE           
#>  5 priority        TRUE             Priority              VARC… FALSE           
#>  6 building_id     TRUE             Building              VARC… FALSE           
#>  7 eqp_no          TRUE             Equipment#            VARC… FALSE           
#>  8 assign_to       TRUE             Assign To             VARC… FALSE           
#>  9 issue_to        TRUE             Issue To              VARC… FALSE           
#> 10 contractor_name TRUE             Contractor Name       VARC… FALSE           
#> # ℹ 29 more rows
```

#### Modifiers

You can use Megamation’s [modifiers](https://apidocs.megamation.com/) on
your field values. For example, `[]` means “containing”. Supply
`<field> = "[]<value>"` to get rows where `<field>` contains `<value>`.
For example, the following requests all work orders containing trades
`ADMIN` and `IT`:

``` r
admin_and_it <- c("[]ADMIN", "[]IT")

mm_get("workorder", trade = admin_and_it)
#> # Data Not Shown for Privacy
```

#### Dates

The `date` field argument only accepts values of type `Date`. The
following requests all work orders from January, 2023:

``` r
jan_2023 <- seq.Date(
  as.Date("2023-01-01"),
  as.Date("2023-01-31"),
  by = "day"
)

mm_get("workorder", date = jan_2023)
#> # Data Not Shown for Privacy
```

## For More

Please see the package website: <https://asadow.github.io/megamation/>.

``` r
end_vignette()
```
