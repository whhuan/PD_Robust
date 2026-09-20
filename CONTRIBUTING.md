# Documentation maintenance

Roxygen comments in `R/` are the sole source for function and package
reference documentation. Edit these comments and run
`roxygen2::roxygenise()` to regenerate the R package help files in
`man/`. Commit the source comments and generated help files together; do
not edit `man/` directly.

The pkgdown GitHub Action builds the reference website from the package
documentation and deploys it to GitHub Pages after a push to `main` or
`master`. Files in `docs/`, including `docs/reference/`, are generated
website output, ignored by Git, and must not be committed to `main` or
maintained manually. The Action generates `docs/` and deploys its
contents to the root of `gh-pages`, which is the GitHub Pages publishing
source. Local website builds are for preview only; publish website
changes through the Action. Vignette sources remain in `vignettes/`, and
website configuration remains in `_pkgdown.yml`.
