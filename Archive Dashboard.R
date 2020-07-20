library(htmltools)
dir <- "C:/Users/lenang01/Documents/UCC-Dashboard"
file.copy(paste0(dir,"/UCC_Dashboard.html"),paste0(dir,"/Dashboard Archive"))
file.rename(paste0(dir,"/Dashboard Archive/UCC_Dashboard.html"),paste0(dir,"/Dashboard Archive/UCC_Dashboard_",Sys.Date(),".html"))
