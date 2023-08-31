# init.R
#
# Example R code to install packages if not already installed
#
install.packages('tidyverse')
my_packages = c('ggforce', 'shinyjs', 'ggdark', 'ggthemes', 'shinydashboard', 'DT', 'markdown','shinythemes')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
