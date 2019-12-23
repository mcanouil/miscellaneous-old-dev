usethis::create_tidy_package("mypackage")


usethis::use_news_md(open = FALSE)
usethis::edit_file("README.Rmd")
rmarkdown::render("README.Rmd",  encoding = 'UTF-8')
unlink("README.html")

usethis::edit_file(".github/SUPPORT.md")

usethis::use_git()
usethis::use_github()
usethis::use_tidy_ci()
usethis::use_appveyor()

usethis::use_pkgdown()
usethis::use_pkgdown_travis()
