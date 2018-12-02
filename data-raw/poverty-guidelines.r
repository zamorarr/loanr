poverty <- readr::read_csv("data-raw/poverty-guidelines.csv")
usethis::use_data(poverty, overwrite = TRUE)
