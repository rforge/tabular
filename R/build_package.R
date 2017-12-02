#setwd("C:/Users/carlin/Documents/r_dev/r_forge/tabular/pkg")
# setwd("C:/r_dev/r_forge/tabulaR/pkg")
setwd("C:/Users/aile/Documents/tabulaR/pkg")

# install.packages("devtools")
library(devtools)
#  install.packages("testthat")
library(testthat)
# install.packages("roxygen2")
library(roxygen2)
find_rtools()
has_devel()
show_news("dprint")
load_all("dprint")

#run_examples("dprint") 
check("dprint")
check_man("dprint")

build(pkg = "dprint",
      #path = "C:/Users/carlin/Documents/VNSNY/dprint",
      path="C:/Users/aile/Documents/r_dev_zipped",
      binary = TRUE)

document("dprint")
run_examples("dprint")
test("dprint")


# export(dprint.lm)