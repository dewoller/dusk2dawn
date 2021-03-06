

safe_load = function( package, repo ="http://cran.us.r-project.org" ) {
  
  if (!require(package, character.only=TRUE)) {
    install.packages(package, repos = repo)
      require(package, character.only=TRUE)
  }
}

detach_all_packages <- function() {
  basic.packages.blank <-  c("stats", 
      "graphics", 
      "grDevices", 
      "utils", 
      "datasets", 
      "methods", 
      "base",
      "nvimcom",
      "colorout")
    basic.packages <- paste("package:", basic.packages.blank, sep = "")

    package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, 
        TRUE, 
        FALSE)]

    package.list <- setdiff(package.list, basic.packages)

    if (length(package.list) > 0)  for (package in package.list) {
      detach(package, character.only = TRUE)
        print(paste("package ", package, " detached", sep = ""))
    }
}


