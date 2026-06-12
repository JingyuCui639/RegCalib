setwd("C:/Users/jc4428/OneDrive - Yale University/Documents/GitHub/RegCalib")

## generate website manual
devtools::document()
devtools::install()

tools::pkg2HTML(
  package = "RegCalib",
  out = "RegCalib.html",
  include_description = TRUE
)

browseURL("RegCalib.html")

#### Website manual 2
install.packages("pkgdown")

pkgdown::build_site()

## generate pdf manual
install.packages("tinytex")
tinytex::install_tinytex()

devtools::build_manual(path = ".")

