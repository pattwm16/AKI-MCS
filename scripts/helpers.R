read_then_csv <- function(sheet, path) {
  pathbase <- path %>%
    basename() %>%
    tools::file_path_sans_ext()
  path %>%
    read_excel(sheet = sheet) %>% 
    write_csv(paste0(pathbase, "-", sheet, ".csv"))
}