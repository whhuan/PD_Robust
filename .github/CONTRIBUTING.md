# Documentation maintenance

Roxygen comments in `R/` are the sole source for function and package reference
documentation. Edit these comments and run `roxygen2::roxygenise()` to regenerate
the R package help files in `man/`. Commit the source comments and generated
help files together; do not edit `man/` directly.

The pkgdown GitHub Action builds the reference website from the package
documentation and deploys it to GitHub Pages after a push to `main` or `master`.
Files in `docs/`, including `docs/reference/`, are generated website output and
must not be maintained manually. Vignette sources remain in `vignettes/`, and
website configuration remains in `_pkgdown.yml`.
