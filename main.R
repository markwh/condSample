# main.r

library(roxygen2)
library(devtools)

# to make my code look pretty
install.packages("formatR")
formatR::tidy_dir("R")


# To make Roxygen documentation:
devtools::document()

use_testthat()

use_package("markstats")
use_package("rcmodel", type = "suggests")
