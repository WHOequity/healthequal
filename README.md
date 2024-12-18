
<!-- README.md is generated from README.Rmd. Please edit that file -->

# healthequal - Calculating summary measures of inequality <img src="man/figures/logo.png" align="right" height="140" alt="healthequal website">

<!-- badges: start -->

[![Project Status:
Concept](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: AGPL (\>=
3)](https://img.shields.io/badge/License-AGPL%20%28%3E%3D%203%29-blue.svg)](https://choosealicense.com/licenses/agpl-3.0/)
[![R-CMD-check](https://github.com/WHOequity/healthequal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WHOequity/healthequal/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/healthequal)](https://CRAN.R-project.org/package=healthequal)
<!-- badges: end -->

The `healthequal` package provides computational tools for calculating
21 summary measures of health inequality.

# Installation

You can install the released version of healthequal from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("healthequal")
```

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("WHOequity/healthequal")
```

## Measures of health inequality included in `healthequal`:

The following summary measures of health inequality are included in the
`healthequal` package:

### Simple measures

- Difference (`d`)
- Ratio (`r`)

### Disproportionality measures (ordered dimensions)

- Absolute concentration index (`aci`)
- Relative concentration index (`rci`)

### Regression-based measures (ordered dimensions)

- Slope index of inequality (`sii`)
- Relative index of inequality (`rii`)

### Variance measures (non-ordered dimensions)

- Between-group variance (`bgv`)
- Between-group standard deviation (`bgsd`)
- Coefficient of variation (`covar`)

### Mean difference measures (non-ordered dimensions)

- Mean difference from mean - unweighted and weighted (`mdmu` and
  `mdmw`)
- Mean difference from best-performing subgroup - unweighted and
  weighted (`mdbu` and `mdbw`)
- Mean difference from reference subgroup - unweighted and weighted
  (`mdru` and `mdrw`)
- Index of disparity - unweighted and weighted (`idisu` and `idisw`)

### Disproportionality measures (non-ordered dimensions)

- Theil index (`ti`)
- Mean log deviation (`mld`)

### Impact measures

- Population attributable risk (`parisk`)
- Population attributable fraction (`paf`)

# Package data

The `healthequal` package comes with sample data for users to be able to
test the package functions. The `OrderedSample` and `NonorderedSample`
data contain data disaggregated by economic status and subnational
region, respectively, for a single indicator.

### Ordered and Nonordered data

``` r
data(OrderedSample)
head(OrderedSample)
```

``` r
data(NonorderedSample)
head(NonorderedSample)
```

### Disagregated data

The `OrderedSampleMultipleind` and `OrderedSampleMultipleind` data
contain disaggregated data by economic status and subnational region,
respectively, for two indicators.

``` r
data(OrderedSampleMultipleind)
head(OrderedSampleMultipleind)
```

``` r
data(NonorderedSampleMultipleind)
head(NonorderedSampleMultipleind)
```

### More info on the datasets

For information about the datasets, type the following commands, which
will display the corresponding dataset help file:

``` r
?healthequal::OrderedSample
?healthequal::NonorderedSample
?healthequal::OrderedSampleMultipleind
?healthequal::NonorderedSampleMultipleind
?healthequal::IndividualSample
```

# References:

Schlotheuber, A, Hosseinpoor, AR. Summary measures of health inequality: 
A review of existing measures and their application. Int J Environ Res Public 
Health. 2022;19(6):3697. [doi:10.3390/ijerph19063697](https://doi.org/10.3390/ijerph19063697)
