# splmaps <a href="https://docs.sykdomspulsen.no/splmaps"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[splmaps](https://docs.sykdomspulsen.no/splmaps) is developed for public health reporting purposes for different geographic levels in Norway.

Contains map data for the following geographic granularities:

- County
- Municipality
- City ward (only for Oslo)

It is convenient to visualise maps with additional information, either using text or color palettes.

Read the introduction vignette [here](http://docs.sykdomspulsen.no/splmaps/articles/splmaps.html) or run `help(package="splmaps")`.

## splverse

<a href="https://docs.sykdomspulsen.no/packages"><img src="https://docs.sykdomspulsen.no/packages/splverse.png" align="right" width="120" /></a>

The [splverse](https://docs.sykdomspulsen.no/packages) is a set of R packages developed to help solve problems that frequently occur when performing infectious disease surveillance.

If you want to install the dev versions (or access packages that haven't been released on CRAN), run `usethis::edit_r_profile()` to edit your `.Rprofile`. 

Then write in:

```
options(
  repos = structure(c(
    FHI  = "https://folkehelseinstituttet.github.io/drat/",
    CRAN = "https://cran.rstudio.com"
  ))
)
```

Save the file and restart R.

You can now install [splverse](https://docs.sykdomspulsen.no/packages) packages from the FHI registry.

```
install.packages("splmaps")
```

