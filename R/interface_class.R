add_class <- function(obj, jltype, rtype) {
  if (jl_isdefined("IAI", jltype)) {
    # Check if obj is the right jltype, or if obj is a GridSearch and the grid
    # learner is the right jltype
    type <- paste0("IAI.", jltype)
    if (jl_isa(obj, type) ||
        (jl_isa(obj, "IAI.GridSearch") && jl_isa(get_learner(obj), type))) {
      class(obj) <- c(rtype, class(obj))
    }
  }
}


set_obj_class <- function(obj) {
  add_class(obj, "Any", "IAIObject")

  stopifnot(c("IAIObject", "JuliaObject") == class(obj))

  # IAIBase
  add_class(obj, "Learner", "learner")
  add_class(obj, "MultiLearner", "multi_learner")
  add_class(obj, "SupervisedLearner", "supervised_learner")
  add_class(obj, "SupervisedMultiLearner", "supervised_multi_learner")
  add_class(obj, "UnsupervisedLearner", "unsupervised_learner")
  add_class(obj, "ClassificationLearner", "classification_learner")
  add_class(obj, "RegressionLearner", "regression_learner")
  add_class(obj, "SurvivalLearner", "survival_learner")
  add_class(obj, "PrescriptionLearner", "prescription_learner")
  add_class(obj, "PolicyLearner", "policy_learner")
  add_class(obj, "ImputationLearner", "imputation_learner")
  add_class(obj, "ClassificationMultiLearner", "classification_learner")
  add_class(obj, "ClassificationMultiLearner", "classification_multi_learner")
  add_class(obj, "RegressionMultiLearner", "regression_learner")
  add_class(obj, "RegressionMultiLearner", "regression_multi_learner")

  # IAITrees
  add_class(obj, "TreeLearner", "tree_learner")
  add_class(obj, "ClassificationTreeLearner", "classification_tree_learner")
  add_class(obj, "RegressionTreeLearner", "regression_tree_learner")
  add_class(obj, "SurvivalTreeLearner", "survival_tree_learner")
  add_class(obj, "PrescriptionTreeLearner", "prescription_tree_learner")
  add_class(obj, "PolicyTreeLearner", "policy_tree_learner")
  add_class(obj, "TreeMultiLearner", "tree_multi_learner")
  add_class(obj, "ClassificationTreeMultiLearner", "classification_tree_learner")
  add_class(obj, "ClassificationTreeMultiLearner", "classification_tree_multi_learner")
  add_class(obj, "RegressionTreeMultiLearner", "regression_tree_learner")
  add_class(obj, "RegressionTreeMultiLearner", "regression_tree_multi_learner")

  # OptimalTrees
  add_class(obj, "OptimalTreeLearner", "optimal_tree_learner")
  add_class(obj, "OptimalTreeClassifier", "optimal_tree_classifier")
  add_class(obj, "OptimalTreeRegressor", "optimal_tree_regressor")
  if (iai_version_less_than("2.0.0")) {  # Avoid depwarning on 2+
    add_class(obj, "OptimalTreeSurvivor", "optimal_tree_survival_learner")
  }
  add_class(obj, "OptimalTreeSurvivalLearner", "optimal_tree_survival_learner")
  add_class(obj, "OptimalTreePrescriptionMinimizer", "optimal_tree_prescription_minimizer")
  add_class(obj, "OptimalTreePrescriptionMaximizer", "optimal_tree_prescription_maximizer")
  add_class(obj, "OptimalTreePolicyMinimizer", "optimal_tree_policy_minimizer")
  add_class(obj, "OptimalTreePolicyMaximizer", "optimal_tree_policy_maximizer")
  add_class(obj, "OptimalTreeMultiLearner", "optimal_tree_multi_learner")
  add_class(obj, "OptimalTreeMultiClassifier", "optimal_tree_multi_classifier")
  add_class(obj, "OptimalTreeMultiRegressor", "optimal_tree_multi_regressor")

  # OptimalFeatureSelection
  add_class(obj, "OptimalFeatureSelectionLearner", "optimal_feature_selection_learner")
  add_class(obj, "OptimalFeatureSelectionClassifier", "optimal_feature_selection_classifier")
  add_class(obj, "OptimalFeatureSelectionRegressor", "optimal_feature_selection_regressor")

  # OptImpute
  add_class(obj, "MeanImputationLearner", "mean_imputation_learner")
  add_class(obj, "RandImputationLearner", "rand_imputation_learner")
  add_class(obj, "SingleKNNImputationLearner", "single_knn_imputation_learner")
  add_class(obj, "ZeroImputationLearner", "zero_imputation_learner")
  add_class(obj, "OptKNNImputationLearner", "opt_knn_imputation_learner")
  add_class(obj, "OptSVMImputationLearner", "opt_svm_imputation_learner")
  add_class(obj, "OptTreeImputationLearner", "opt_tree_imputation_learner")

  # Heuristics
  add_class(obj, "RandomForestLearner", "random_forest_learner")
  add_class(obj, "RandomForestClassifier", "random_forest_classifier")
  add_class(obj, "RandomForestRegressor", "random_forest_regressor")
  add_class(obj, "RandomForestSurvivalLearner", "random_forest_survival_learner")

  add_class(obj, "XGBoostLearner", "xgboost_learner")
  add_class(obj, "XGBoostClassifier", "xgboost_classifier")
  add_class(obj, "XGBoostRegressor", "xgboost_regressor")
  add_class(obj, "XGBoostSurvivalLearner", "xgboost_survival_learner")

  add_class(obj, "GLMNetLearner", "glmnet_learner")
  if (iai_version_less_than("3.0.0")) {
    add_class(obj, "GLMNetCVRegressor", "glmnet_learner")
  } else if (iai_version_less_than("3.1.0")) {
    add_class(obj, "GLMNetCVLearner", "glmnet_learner")
  }

  add_class(obj, "GLMNetCVLearner", "glmnetcv_learner")
  if (iai_version_less_than("3.0.0")) {
    add_class(obj, "GLMNetCVRegressor", "glmnetcv_learner")
  }

  add_class(obj, "GLMNetCVClassifier", "glmnetcv_classifier")
  add_class(obj, "GLMNetCVRegressor", "glmnetcv_regressor")
  add_class(obj, "GLMNetCVSurvivalLearner", "glmnetcv_survival_learner")

  # RewardEstimation
  if (iai_version_less_than("2.1.0")) {  # Avoid depwarning on 2+
    add_class(obj, "RewardEstimator", "reward_estimator")
    add_class(obj, "RewardEstimator", "categorical_reward_estimator")
  } else if (iai_version_less_than("2.2.0")) {
    add_class(obj, "CategoricalRewardEstimator", "reward_estimator")
    add_class(obj, "NumericRewardEstimator", "reward_estimator")
  } else {
    add_class(obj, "RewardEstimator", "reward_estimator")
  }

  add_class(obj, "CategoricalRewardEstimator", "categorical_reward_estimator")
  add_class(obj, "CategoricalClassificationRewardEstimator", "categorical_classification_reward_estimator")
  add_class(obj, "CategoricalRegressionRewardEstimator", "categorical_regression_reward_estimator")
  add_class(obj, "CategoricalSurvivalRewardEstimator", "categorical_survival_reward_estimator")

  add_class(obj, "NumericRewardEstimator", "numeric_reward_estimator")
  add_class(obj, "NumericClassificationRewardEstimator", "numeric_classification_reward_estimator")
  add_class(obj, "NumericRegressionRewardEstimator", "numeric_regression_reward_estimator")
  add_class(obj, "NumericSurvivalRewardEstimator", "numeric_survival_reward_estimator")

  add_class(obj, "EqualPropensityEstimator", "equal_propensity_estimator")

  # Misc
  add_class(obj, "AbstractVisualization", "abstract_visualization")

  add_class(obj, "MultiQuestionnaire", "multi_questionnaire")
  add_class(obj, "MultiTreePlot", "multi_tree_plot")
  add_class(obj, "Questionnaire", "questionnaire")
  add_class(obj, "ROCCurve", "roc_curve")
  add_class(obj, "SurvivalCurve", "survival_curve")
  add_class(obj, "SimilarityComparison", "similarity_comparison")
  add_class(obj, "StabilityAnalysis", "stability_analysis")
  add_class(obj, "TreePlot", "tree_plot")

  # GridSearch
  add_class(obj, "GridSearch", "grid_search")

  obj
}
