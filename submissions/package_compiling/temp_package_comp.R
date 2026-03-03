usethis::create_package("/University/Year4/STA380/simSeriesACF/")

usethis::use_testthat(3)

devtools::document()

devtools::build_vignettes()

usethis::use_vignette("my-vignette")

devtools::check()
