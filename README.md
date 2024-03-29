
# megamation <a href="https://asadow.github.io/megamation/"><img src="man/figures/logo.png" alt="megamation website" align="right" height="139"/></a>

**Authors:** [Adam Sadowski](https://adams.quarto.pub/w/)<br/>
**License:** [GPL (\>= 3)](https://www.gnu.org/licenses/licenses.html)

<!-- badges: start -->

[![R-CMD-check](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/asadow/megamation/branch/master/graph/badge.svg)](https://app.codecov.io/gh/asadow/megamation?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

You have two options.

Either use `mm_authorize()`:

``` r
mm_authorize(
  key = "<YOUR-MEGAMATION_KEY>",
  url = "<YOUR-MEGAMATION_URL>"
)
```

Or run `usethis::edit_r_environ()` to install your API credentials in
your `.Renviron`.

``` r
MEGAMATION_KEY = '<YOUR-MEGAMATION_KEY>'
MEGAMATION_URL = '<YOUR-MEGAMATION_URL>'
```

Now `megamation` functions will automatically use your credentials, so
you don’t have to expose them in your code.

## Grabbing Data

`mm_data()` downloads data from your tables into R.

``` r
mm_data("status")
#> ℹ Requesting...
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

It provides informative R errors (alongside HTTP errors).

``` r
mm_data("statuses")
#> ℹ Requesting...
#> ℹ NB: 1/1 requests returned errors or no data.
#> 
#> ── Errored ─────────────────────────────────────────────────────────────────────
#> ✖ HTTP 404 Not Found.
#> • This is not a valid web API endpoint. 
#> GET https://api.megamation.com/uog/dl/statuses?ALLFIELDS=1
#> # A tibble: 0 × 0
```

### Filtering

`mm_data()` allows you to easily filter by fields (columns).

#### Available Filters

`mm_names()` allows you to see the columns that can act as filters.

``` r
mm_names("workorder")
#> # A tibble: 39 × 4
#>    field           filter_enabled description               type          
#>    <chr>           <lgl>          <chr>                     <chr>         
#>  1 type            TRUE           Type                      VARCHAR(65531)
#>  2 status          TRUE           Status                    VARCHAR(65531)
#>  3 date            TRUE           Date - [Month][Day][Year] DATE          
#>  4 submitted_by    TRUE           Submitted By              VARCHAR(65531)
#>  5 priority        TRUE           Priority                  VARCHAR(65531)
#>  6 building_id     TRUE           Building                  VARCHAR(65531)
#>  7 eqp_no          TRUE           Equipment#                VARCHAR(65531)
#>  8 assign_to       TRUE           Assign To                 VARCHAR(65531)
#>  9 issue_to        TRUE           Issue To                  VARCHAR(65531)
#> 10 contractor_name TRUE           Contractor Name           VARCHAR(255)  
#> # ℹ 29 more rows
```

#### Modifiers

You can use Megamation’s [modifiers](https://apidocs.megamation.com/) on
filter values.

- `[]` for containing

- `!!` for not equal

- `>>` for greater than

- `<<` for less than

- `<>` for between.

For example, supply `<field> = "[]<value>"` to get rows where `<field>`
contains `<value>`. Here’s how you would request all work orders
containing trades `ADMIN` and `IT`.

``` r
admin_and_it <- c("[]ADMIN", "[]IT")

mm_data("workorder", trade = admin_and_it)
#> # Data Not Shown for Privacy
```

#### Dates

Avoid `<>` for dates. Instead, use a sequence of R `Date`’s. Here’s how
you would request all work orders from January, 2023.

``` r
jan_2023 <- seq.Date(
  as.Date("2023-01-01"),
  as.Date("2023-01-31"),
  by = "day"
)

mm_data("workorder", date = jan_2023)
#> # Data Not Shown for Privacy
```

## For More

Please see the package website: <https://asadow.github.io/megamation/>.

## Code of Conduct

Please note that the megamation project is released with a [Contributor
Code of
Conduct](https://asadow.github.io/megamation/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms. Feedback,
bug reports (and fixes!), and feature requests are welcome; file issues
or seek support [here](https://github.com/asadow/megamation/issues).
