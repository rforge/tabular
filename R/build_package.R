setwd("C:/Users/carlin/Documents/r_dev/r_forge/tabular/pkg")
#setwd("C:/r_dev/r_forge/tabulaR/pkg")

library(devtools)
library(testthat)
library(roxygen2)
has_devel()
show_news("dprint")
load_all("dprint")

check("dprint")
check_doc("dprint")
run_examples("dprint")

build(pkg = "dprint",
      path = "C:/VNSNY/dprint",
      #path="C:/r_dev/zipped",
      binary = TRUE)

document("dprint")
run_examples("dprint")
test("dprint")