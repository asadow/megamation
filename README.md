
# megamation <a href="https://asadow.github.io/megamation/"><img src="man/figures/logo.png" alt="megamation website" align="right" height="139"/></a>

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

<!-- ## Installation -->
<!-- You can install the released version of megamation from [CRAN](https://cran.r-project.org/) with: -->
<!-- ```{r, eval = FALSE} -->
<!-- install.packages("megamation") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->
<!-- ```{r, eval = FALSE} -->
<!-- # install.packages("pak") -->
<!-- pak::pak("asadow/megamation") -->
<!-- ``` -->
<!-- ## Attach megamation -->
<!-- ```{r} -->
<!-- library(megamation) -->
<!-- ``` -->
<!-- ## Authorization -->
<!-- `mm_set_creds()` installs your API credentials: -->
<!-- ``` {r, eval = FALSE} -->
<!-- mm_set_creds( -->
<!--    key = "<YOUR-MEGAMATION_KEY>", -->
<!--    url = "<YOUR-MEGAMATION_URL>" -->
<!--    ) -->
<!-- ``` -->
<!-- ## Get Data -->
<!-- `mm_get()` makes a GET request to an endpoint: -->
<!-- ```{r} -->
<!-- mm_get("status") -->
<!-- ``` -->
<!-- It provides informative R errors (alongside HTTP errors): -->
<!-- ```{r, error = TRUE} -->
<!-- mm_get("statuses") -->
<!-- ``` -->
<!-- ### Filter Requests -->
<!-- `mm_get()` allows you to easily filter requests by fields.  -->
<!-- Let's try to request two work orders by their work order numbers: -->
<!-- ```{r, error = TRUE} -->
<!-- mm_get("workorder", wo_no = c("00001", "00002")) -->
<!-- ``` -->
<!-- Uh oh! `wo_no` is not available as a filterable field (yet). You can ask your Megamation representative to add it. -->
<!-- #### Available Filters -->
<!-- Supply `opts = req_opts(.get = "criteria")` to see which of your fields are currently filter-enabled: -->
<!-- ```{r} -->
<!-- mm_get("workorder", opts = req_opts(.get = "criteria")) -->
<!-- ``` -->
<!-- #### Modifiers -->
<!-- You can use Megamation's [modifiers](https://apidocs.megamation.com/) on your field values. For example, `[]` means "containing". Supply `<field> = "[]<value>"` to get rows where `<field>` contains `<value>`. For example, the following requests all work orders containing trades `ADMIN` and `IT`: -->
<!-- ```{r, eval = FALSE} -->
<!-- admin_and_it <- c("[]ADMIN", "[]IT") -->
<!-- mm_get("workorder", trade = admin_and_it) -->
<!-- #> # Data Not Shown for Privacy -->
<!-- ``` -->
<!-- #### Dates -->
<!-- The `date` field argument only accepts values of type `Date`. The following requests all work orders from January, 2023: -->
<!-- ```{r, eval = FALSE} -->
<!-- jan_2023 <- seq.Date( -->
<!--   as.Date("2023-01-01"),   -->
<!--   as.Date("2023-01-31"),  -->
<!--   by = "day" -->
<!--   ) -->
<!-- mm_get("workorder", date = jan_2023) -->
<!-- #> # Data Not Shown for Privacy -->
<!-- ``` -->
<!-- ## For More -->
<!-- Please see the package website: <https://asadow.github.io/megamation/>. -->
<!-- ```{r} -->
<!-- end_vignette() -->
<!-- ``` -->
