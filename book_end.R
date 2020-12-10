# Create a bibliography with all the packages we are using.

# It would be nice if this only ran when we are building the whole book. How can
# we check that?

knitr::write_bib(c(.packages(), "bookdown"), "bib/packages.bib")

# Running snapshot from renv after a successful build to update necessary
# packages. This used to work fine without the force argument, but started
# failing, at least with chapter 6, with a message about "pre-flight validation failure".

# renv::snapshot(force = TRUE)
