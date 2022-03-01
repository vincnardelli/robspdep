# setup
usethis::use_build_ignore("dev")
usethis::use_data_raw()
devtools::document()
# Description
usethis::use_description(
  list(
    Title = "robspdep: Robust Spatial Dependence",
    `Authors@R` = "c(person('Vincenzo', 'Nardelli', email = 'vincnardelli@gmail.com', role = c('cre', 'aut')))",
    Description = "A collection of functions and tests for robust spatial autocorrelation.
    It include global and local Moran-Huber I.",
    URL = "https://github.com/vincnardelli/robspdep"
  )
)
usethis::use_lgpl_license()
usethis::use_tidy_description()

usethis::use_package("boot")
usethis::use_package("stats")
usethis::use_package("spdep")
# usethis::use_pipe(export = TRUE)
# usethis::use_package("dplyr")
# usethis::use_package("sf")
# usethis::use_package("ggplot2")
# usethis::use_package("spdep")
# usethis::use_package("purrr")
# usethis::use_package("parallel")
# usethis::use_package("progress")
# usethis::use_package("gganimate")
# usethis::use_package("testthat", "Suggest")
# usethis::use_package("utils", "Suggest")
# usethis::use_package("methods")
# usethis::use_package("stats")
# usethis::use_vignette(name="epidsampler")


# Read me
usethis::use_readme_md( open = FALSE )


# Test that
# usethis::use_testthat()
# usethis::use_test("map_generation")
# usethis::use_test("map_step")


usethis::use_git_config(
  scope = "user",
  user.name = "Vincenzo Nardelli",
  user.email = "vincnardelli@gmail.com"
)
usethis::use_git()


# CI
# usethis::use_travis()
# usethis::use_coverage()
# usethis::use_lifecycle_badge("experimental")
# usethis::use_github_action_check_standard(save_as="test.yaml")
# usethis::use_github_action("test-coverage")

# Website
# usethis::use_pkgdown()
# pkgdown::build_site()
# usethis::use_github_action("pkgdown")
