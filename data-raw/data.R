codes <- readxl::read_excel("inst/extdata/codes.xlsx")
usethis::use_data(codes, overwrite=TRUE)
