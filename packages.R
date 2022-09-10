
installed_packages <- installed.packages()
needed_packages <- c("jsonlite", "dplyr", "dbplyr", "zoo", "curl", "plyr")

for(i in 1:length(needed_packages)){
  if(needed_packages[i] %in% installed_packages){
    print(paste(needed_packages[i], "already installed"))
  }else{
    install.packages(needed_packages[i])
  }
}

library(jsonlite)
library(dplyr)
library(dbplyr)
library(zoo)

getwd()

