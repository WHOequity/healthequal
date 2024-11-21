# healthequal 1.0.1

## Bug fixes

* Fix confidence interval simulation for non-ordered dimensions when using non-percentage indicators.
* Remove row names for non-ordered dimensions with simulated confidence intervals.
* Fix `d` to run without declared standard errors (`se`).
* Fix `d` and `r` to select correct subgroup reference values for non-ordered dimensions with/without reference group. 
* Fix `d` and `r` to return positive results and ≥1 for data with non-ordered dimensions without reference group, respectively.
* Fix scale of the reported `se` for `sii`.
* Allow `conf.level` for `sii` and `rii` with aggregated data.
* Use `family=quasibinomial(logit)` for `sii` and `rii` non-linear regression models.
* Add example of the use of `sii`, `rii`, `aci` and `rci` with disaggregated (OrderedSample) data. 
* Add additional variable checks.
* Add `force` option to all summary measures. 
* Remove requirement for declared `scale` variable for summary measures with simulated confidence intervals.
* Drop creation of `scaleval` in `rci`. 
* Add `se` to all return data frames. 

## Text and formatting

* Standardise formatting. 
* Add additional references and apply WHO reference style.
* Edit summary measure descriptions. 

# healthequal 1.0.0 

* Added 'NEWS.md` file to track changes to the package.
