# iai 1.3.0

* Add support for Optimal Policy Trees
* Add support for `reward_estimator` for prescription problems
* Add `predict_expected_survival_time` for survival learners
* Add direct `roc_curve` calculation from probabilities and labels
* `optimal_tree_survivor` has been deprecated and renamed to `optimal_tree_survival_learner` to match the IAI Julia interface.

# iai 1.2.0

* Allow specifying Julia location with `IAI_JULIA` environment variable
* Fixes to ensure `iai_setup` is run before any Julia functions are called
* Support for CategoricalArrays.jl v0.7.7
* Add `predict_hazard` for survival learners

# iai 1.1.0

* Add API for Optimal Feature Selection and advanced tree visualization
* Display visualizations in the viewer if available
* Update titles of documentation pages
* No longer need to run `iai_setup()` before using `iai` functions

# iai 1.0.0

* Initial release
