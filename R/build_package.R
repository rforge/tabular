#setwd("C:/Users/carlin/Documents/r_dev/r_forge/tabular/pkg")
setwd("C:/r_dev/r_forge/tabulaR/pkg")

# install.packages("devtools")
library(devtools)
#  install.packages("testthat")
library(testthat)
# install.packages("roxygen2")
library(roxygen2)
has_devel()
show_news("dprint")
load_all("dprint")

check("dprint")
check_doc("dprint")
run_examples("dprint")

build(pkg = "dprint",
      #path = "C:/Users/carlin/Documents/VNSNY/dprint",
      path="C:/r_dev/zipped",
      binary = TRUE)

document("dprint")
run_examples("dprint")
test("dprint")