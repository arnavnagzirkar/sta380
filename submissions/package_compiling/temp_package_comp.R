usethis::create_package("/University/Year4/STA380/simSeriesACF/")

usethis::use_testthat(3)

usethis::use_vignette("my-vignette")

devtools::document()

devtools::build_vignettes()

devtools::check()
