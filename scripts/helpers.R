read_then_csv <- function(sheet, path) {
  pathbase <- path %>%
    basename() %>%
    tools::file_path_sans_ext()
  path %>%
    read_excel(sheet = sheet) %>% 
    write_csv(paste0(pathbase, "-", sheet, ".csv"))
}

# SMD FUNCTION------------------------------------------------------------------
SMD_value <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables
    try({a<-data.frame(y)
    a$g<-g
    smd<-(as.data.frame(stddiff.numeric(data=a,gcol = "g", vcol = "y")))$stddiff
    },silent=TRUE)
  } else {
    # For categorical variables
    try({
      a<-data.frame(y)
      a$g<-g
      smd<-(abs((bal.tab(a, treat = "g",data=a,binary="std",continuous =         
                           "std",s.d.denom = "pooled",stats=c("mean.diffs"))$Balance)$Diff.Un))
    },silent=TRUE)
  }
  c("",format(smd,digits=2)) #Formatting number of digits
}
