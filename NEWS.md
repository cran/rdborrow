# rdborrow 0.0.5.0

## Breaking changes
- Formulas in `ec_ipw()`, `ec_aipw()`, `did_ec_ipw()`, `did_ec_aipw()`, and `did_ec_or()` now give an error when the right-hand side uses `.` or a variable that is not in `covariates_col_name`. `.` added the outcomes and the treatment to the model (#105), and a variable not in the data was taken from the R session (#107). An outcome used as a predictor, such as `y1` in the model for `y2`, now gives a warning: it is measured after randomization, so adjusting for it can bias the treatment effect. The primary simulation vignette no longer does this.
- Outcome formulas in `ec_aipw()`, `did_ec_aipw()`, and `did_ec_or()` must now have the outcome name alone on the left-hand side. A transformed left side, such as `log(y1) ~ x1`, was accepted, but `did_ec_or()` used the transformed scale while `ec_aipw()` and `did_ec_aipw()` mixed it with the raw outcome, so the estimates were wrong. Transform the outcome column before the analysis instead. One-sided formulas such as `~ x1` now fail with a clear message (#138, #139).
- `setup_simulation_primary()` and `setup_simulation_OLE()` now require `true_effect` to be a single number, the true effect at the final visit. `run_simulation()` scores only the final visit, so a vector was recycled across simulated trials rather than matched to visits, and bias, coverage, type I error, and power were silently wrong unless every element was equal. With an even number of trials there was no warning (#90).

## Minor improvements
- New example dataset `SyntheticDataII`, an unbalanced companion to `SyntheticData`: 2:1 randomization (160 treated, 80 controls), 140 external controls, and a covariate shift between the trial and the external controls. Balanced data can hide errors, such as exchanging the randomization probability and its complement (#102).
- Progress output from `quiet = FALSE` in `run_analysis()`, `estimate()`, and `run_simulation()` now uses `message()` instead of `cat()`, so `suppressMessages()` silences it (#129).
- `did_ec_ipw()`, `did_ec_aipw()`, `did_ec_or()`, and `scm()` results now include a `standard_deviation` column, the standard deviation of the bootstrap replicates, as the primary methods do with bootstrap inference (#129).
- `ec_ipw()` and `ec_aipw()` now compute the sandwich variance without an N x N matrix, so memory use grows linearly with the sample size. At N = 8,000, peak memory falls from about 650 MB to 185 MB, and N = 50,000 runs in 0.2 seconds instead of 20. The results are unchanged (#117).
- `scm()` now solves its optimizations with CVXR's current interface (`psolve()` and `value()`) instead of `solve()` and `$getValue()`, which CVXR has deprecated and will remove. rdborrow now requires CVXR >= 1.8.1. Estimates are unchanged (#97).
- `setup_analysis_primary()` now returns its analysis object visibly, as `setup_analysis_OLE()` does, so that it prints at the console (#129).

## Bug fixes
- All method constructors now require `bootstrap` to be at least 2. `bootstrap = 1` was accepted and then failed inside `run_analysis()` with an opaque confidence interval error (#91).
- `ec_aipw()` now computes its sandwich variance from the outcome models of the point estimate. It fitted them again on all patients, so a formula with a data-dependent basis such as `splines::ns()` got different knots, and the standard error was slightly off (#120).
- `ec_aipw()`, `did_ec_aipw()`, and `did_ec_or()` now match each outcome formula to an outcome by its left-hand side. They used the formulas in list order, so formulas listed in a different order from the outcomes silently gave wrong estimates; in `did_ec_or()`, reversing them flipped the signs. A formula for a variable that is not an outcome, two formulas for one outcome, or an outcome with no formula is now an error (#104).
- `ec_ipw()`, `ec_aipw()`, `did_ec_ipw()`, and `did_ec_aipw()` now work when the trial-status column is not named `S`, and `trt_formula` works when the treatment column is not named `A`. The formula's left-hand side was rewritten to the user's column name after the data had been copied into columns named `S` and `A`, so the model could not find it, or silently used a same-named object from the R session (#103).
- `ec_ipw()` and `ec_aipw()` now give an error when `bootstrap_ci_type` is set without `bootstrap`. The value was kept but not used, so a user who asked for, say, BCa intervals got sandwich intervals with no message (#123).
- `ec_ipw()` and `ec_aipw()` no longer stop with a LAPACK "system is exactly singular" error when a covariate is collinear with others or constant. `glm()` and `lm()` already drop such a covariate from the fit, but the sandwich variance kept its column. The results now equal those without the covariate (#118).
- `run_analysis()` now checks that its first argument is an analysis object and that `quiet` is `TRUE` or `FALSE`. A wrong type gave "no applicable method for `@`" (#125).
- `run_simulation()` no longer errors with "'x' is NULL" when a method's results have a single column. It kept the last row of each replicate without `drop = FALSE`, so a one-column data frame collapsed to a vector (#93).
- `scm()` now works with two external controls, and gives a clear error with one. Its leave-one-out cross-validation left a single external control, which R turned into a vector, and the analysis failed with "'x' must be an array of at least two dimensions" (#115).
- `scm()` now gives an error that names a covariate that is not numeric or logical, such as a factor. It failed with "Cannot convert object of class <matrix/array> to a CVXR Expression", because the matching matrix became a character matrix (#116).
- `scm()` now rejects an infinite `lambda_min` or `lambda_max`, which failed later in the analysis, and gives an error when `nlambda = 1` and `lambda_max` differs from `lambda_min`, because only `lambda_min` was used. The result of `run_analysis()` now has an attribute `"lambda"` with the penalty that cross-validation selected (#124).
- `scm()` now checks the status of each optimization. When the solver does not find a solution, for example with a covariate in very different units such as age in days, the analysis stops with an error that names the status and suggests rescaling. It stopped later with "requires numeric/complex matrix/vector arguments". Solutions that CVXR marks as inaccurate are still used, with CVXR's warning (#112).
- `setup_analysis_OLE()` and the OLE methods now round a `T_cross` that is a whole number only up to floating-point error, such as `0.6 / 0.2`. It passed validation but was used unrounded, so one visit fell in both periods and `did_ec_ipw()` and `did_ec_aipw()` gave silently wrong estimates. Calling `estimate()` directly now also validates `T_cross`. `simulate_trial()` and `simulate_outcome_from_model()` round `T_cross` the same way, so simulated data no longer starts the crossover one visit early (#110).
- `setup_analysis_primary()` and `setup_analysis_OLE()` now require the trial-status and treatment columns to be numeric or logical. A factor with levels such as `c("1", "0")` passed the 0/1 check, and `trt_formula` then modeled the wrong level, so `did_ec_ipw()` and `did_ec_aipw()` gave silently wrong estimates. Calling `estimate()` directly runs the same check (#111).
- `setup_analysis_primary()` and `setup_analysis_OLE()` now give an error when an external control has treatment 1. The methods assume that external controls are untreated, and `ec_ipw()` and `ec_aipw()` used such patients in different ways: `ec_ipw()` kept them as controls, and `ec_aipw()` left them out of its outcome model. Calling `estimate()` directly runs the same check (#108).
- `setup_analysis_primary()` and `setup_analysis_OLE()` now give an error when an outcome or covariate column has missing values, and name the first column and row. The methods gave `NA` estimates, a finite confidence interval next to an `NA` estimate, or, in `did_ec_or()`, an estimate from the complete cases for some visits only, with no message. Remove or impute missing values before the analysis. Calling `estimate()` directly runs the same check (#114).
- `setup_analysis_primary()`, `setup_analysis_OLE()`, `setup_simulation_primary()`, and `setup_simulation_OLE()` now require `alpha` to be more than 0 and less than 1. `alpha = 0` and `alpha = 1` were accepted and gave infinite, missing, or zero-width confidence intervals; with bootstrap and `alpha = 1`, the interval did not contain the point estimate. Calling `estimate()` directly runs the same check (#121).
- `setup_analysis_primary()` and `setup_analysis_OLE()` now give an error that names the empty group when the data has no trial treated patients or no trial controls, and `setup_analysis_OLE()` also when it has no external controls. The methods gave `NaN` results or an unclear error. The primary methods check for external controls when they run; `ec_ipw(weight = 0)` does not use them, so it still runs without them. Calling `estimate()` directly runs the same checks (#122).
- `setup_analysis_primary()` and `setup_analysis_OLE()` now give an error when an outcome or covariate is named `S` or `A`, or is the trial-status or treatment column. The internal data uses `S` and `A`, so an outcome named `A` with the treatment in another column was read as the treatment, and the estimate was silently wrong. Calling `estimate()` directly runs the same check (#106).
- `setup_analysis_primary()` and `setup_analysis_OLE()` now give an error when an outcome or covariate name is listed twice, including as both an outcome and a covariate. `outcome_col_name = c("y1", "y1")` gave two identical estimates (#125).
- `setup_analysis_primary()` and `setup_analysis_OLE()` now accept outcome and covariate names that are not syntactic R names, such as `"week 12"`. The internal data renamed them (to `week.12`), and the analysis failed with "undefined columns selected". Write such names with backticks in formulas (#125).
- `setup_simulation_OLE()` now checks `T_cross` as `setup_analysis_OLE()` does: it must be a whole number of at least 1 and less than the number of outcomes. An invalid value was accepted, and the error came later from `run_simulation()` (#141).

## Documentation
- Two new developer articles. "Adding a new method" explains how an estimator plugs into `run_analysis()` and `run_simulation()` through S4 dispatch, walks through a runnable toy method, and gives a checklist for adding a method to the package (#87). "Developing with AI coding agents" explains the new `AGENTS.md` file of agent instructions and gives a workflow, example prompts, and review habits for working on the package with an AI coding agent (#88).

# rdborrow 0.0.4.2

## Bug fixes
- Two tests asserted exact floating-point equality between a marginal treatment model and an equivalent intercept-only model. The two are mathematically identical but computed by different routes, so they can differ in the last bit under BLAS libraries such as BLIS; both now compare with a tolerance.

# rdborrow 0.0.4.1

## Breaking changes
- `bootstrap_ci_type` no longer accepts `"stud"` in any method constructor. Studentized intervals require a variance estimate for each bootstrap replicate, which the estimators do not produce, so the option failed whenever it was used (#77).

## Bug fixes
- Corrected the DOIs in the package references. The two DOIs cited in `DESCRIPTION` pointed to unrelated papers, and the open-label extension methods and vignettes cited an unrelated paper rather than Zhou X, Pang H, Drake C, Burger HU, Zhu J (2024). Reported by Herb Pang.
- `bootstrap_ci_type = "bca"` no longer errors. `boot.ci()` re-invokes the bootstrap statistic through `empinf()` without the arguments passed to `boot()`, so those are now captured in a closure (#77).
- `bootstrap_ci_type = "norm"` no longer returns `NA` confidence bounds. The normal component of `boot::boot.ci()` output has three columns rather than five, so the fixed index used to read the bounds ran past the end of it (#77).
- `did_ec_ipw()` and `did_ec_aipw()` no longer return `NA` estimates when `trt_formula` is left at its default of `NULL`. The marginal randomization probability was a scalar, so subsetting the treatment weights by a length-N logical produced `NA` for all but the first subject (#71).

# rdborrow 0.0.4.0

## Breaking changes
- `setup_bootstrap()` has been removed. Bootstrap settings are now specified directly in method constructors (e.g., `ec_ipw(bootstrap = 500, bootstrap_ci_type = "perc")`). Calling `setup_bootstrap()` now raises an error with migration instructions.
- `setup_method_weighting()`, `setup_method_DID()`, and `setup_method_SCM()` have been removed. Use `ec_ipw()`, `ec_aipw()`, `did_ec_ipw()`, `did_ec_aipw()`, `did_ec_or()`, or `scm()` instead. Calling the old constructors now raises an error with migration instructions.
- `EC_IPW_OPT()`, `EC_AIPW_OPT()`, and all `legacy_DID_EC_*` / `legacy_SCM*` internal functions have been deleted.
- `did_ec_ipw()` and `did_ec_aipw()`: `trt_formula` default changed from `""` to `NULL`.

## Changes
- `ec_ipw()` and `ec_aipw()` QC'd against Zhou et al. (2024a, JRSS-A). Internal code now maps directly to paper equations (Def 1/2, Eq 6/7, Theorems 3/4).
- All estimator internals refactored: redundant parameters removed, `potential` trick replaced with direct weighted means, consistent naming (`_core`, `_se`, `_boot_statistic`).
- `build_analysis_df()` is now an S4 method on `method_weighting_obj`.
- Bootstrap logic moved from `.format_primary_results()` (deleted) to inline control flow in each `estimate()` method.
- Added input validation: S/A must be binary 0/1, `T_cross` must be a positive integer less than the number of outcomes, `outcome_formula` length must match number of outcomes.
- Vignette improvements: `T_cross` semantics documented, `head()` calls added, rank-deficiency warnings suppressed in OLE simulation output.

# rdborrow 0.0.3.0

## Changes
- New API introduced: eg `ec_ipw()` and `ec_aipw()` replace `setup_method_weighting()`. 
- Internally, core functions such as `EC_IPW_OPT()` are further broken down into atomic functions.
- Bootstrapping functions now call atomic functions (instead of similar, duplicated code).
- Additional test cases in preparation of deprecating legacy_ functions such as `EC_IPW_OPT()`.
- Bug fixes (AIPW called the IPW bootstrapping function)

# rdborrow 0.0.2.0

## Changes
- Refactor out imports, prepare for CRAN

# rdborrow 0.0.1.0

## Changes
- Original package with both IPW and AIPW methods