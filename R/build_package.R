setwd("C:/Users/carlin/Documents/r_dev/r_forge/tabular/pkg")
library(devtools)
library(testthat)
library(roxygen2)
has_devel()

load_all("dprint")

check("dprint")
check_doc("dprint")
run_examples("dprint")

build(pkg = "dprint", path = "C:/VNSNY/dprint", binary = TRUE)

document("dprint")
run_examples("dprint")
test("dprint")