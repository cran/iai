# Unreleased

# iai 1.9.0

* Add `standard_error` functions for Policy Learners

# iai 1.8.0

* Introduce more generic functions to better cover and match Julia functionality
* Add `resume_from_checkpoint`
* Add `acquire_license`/`release_license`

# iai 1.7.0

* Add `cleanup_installation`
* Deprecate `impute` and `impute_cv`
* Add `predict_reward`
* Use official julialang.org binaries in `install_julia`
* Support additional args in `variable_importance`
* Add `get_machine_id`
* Add `glmnetcv_classifier`
* Add direct score calculation
* Add tree refitting functions

# iai 1.6.0

* Add `add_julia_processes`
* Add tree stability analysis functions
* Add `get_features_used`
* Add support for IAI v2.2 RewardEstimation interface
* Add `predict_shap`
* Add `plot`/`autoplot` functions for `roc_curve` and `grid_search`

# iai 1.5.0

* Add `install_julia` and `install_system_image` for automatic installation
* Add `get_grid_result_details`
* Rename `get_grid_results` to `get_grid_result_summary`

# iai 1.4.0

* Add support for IAI v2.1 RewardEstimation interface
* Add support for Heuristics
* Add `get_roc_curve_data`
* Add `predict_treatment_rank` and `predict_treatment_outcome`
* Add `get_policy_treatment_outcome`, `get_survival_expected_time`, and `get_survival_hazard`
* Add `write_pdf` and `write_svg` to output trees as static images

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
