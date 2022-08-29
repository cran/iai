#' Return the number of nodes in a trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_num_nodes}{\code{IAI.get_num_nodes}}
#'
#' @param lnr The learner to query.
#'
#' @examples \dontrun{iai::get_num_nodes(lnr)}
#'
#' @export
get_num_nodes <- function(lnr) {
  jl_func("IAI.get_num_nodes_convert", lnr)
}


#' Check if a node of a tree is a leaf
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.is_leaf}{\code{IAI.is_leaf}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_leaf(lnr, 1)}
#'
#' @export
is_leaf <- function(lnr, node_index) {
  jl_func("IAI.is_leaf_convert", lnr, node_index)
}


#' Get the depth of a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_depth}{\code{IAI.get_depth}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_depth(lnr, 1)}
#'
#' @export
get_depth <- function(lnr, node_index) {
  jl_func("IAI.get_depth_convert", lnr, node_index)
}


#' Get the number of training points contained in a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_num_samples}{\code{IAI.get_num_samples}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_num_samples(lnr, 1)}
#'
#' @export
get_num_samples <- function(lnr, node_index) {
  jl_func("IAI.get_num_samples_convert", lnr, node_index)
}


#' Get the index of the parent node at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_parent}{\code{IAI.get_parent}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_parent(lnr, 2)}
#'
#' @export
get_parent <- function(lnr, node_index) {
  jl_func("IAI.get_parent_convert", lnr, node_index)
}


#' Get the index of the lower child at a split node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_lower_child}{\code{IAI.get_lower_child}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_lower_child(lnr, 1)}
#'
#' @export
get_lower_child <- function(lnr, node_index) {
  jl_func("IAI.get_lower_child_convert", lnr, node_index)
}


#' Get the index of the upper child at a split node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_upper_child}{\code{IAI.get_upper_child}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_upper_child(lnr, 1)}
#'
#' @export
get_upper_child <- function(lnr, node_index) {
  jl_func("IAI.get_upper_child_convert", lnr, node_index)
}


#' Check if a node of a tree applies a parallel split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.is_parallel_split}{\code{IAI.is_parallel_split}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_parallel_split(lnr, 1)}
#'
#' @export
is_parallel_split <- function(lnr, node_index) {
  jl_func("IAI.is_parallel_split_convert", lnr, node_index)
}


#' Check if a node of a tree applies a hyperplane split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.is_hyperplane_split}{\code{IAI.is_hyperplane_split}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_hyperplane_split(lnr, 1)}
#'
#' @export
is_hyperplane_split <- function(lnr, node_index) {
  jl_func("IAI.is_hyperplane_split_convert", lnr, node_index)
}


#' Check if a node of a tree applies a categoric split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.is_categoric_split}{\code{IAI.is_categoric_split}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_categoric_split(lnr, 1)}
#'
#' @export
is_categoric_split <- function(lnr, node_index) {
  jl_func("IAI.is_categoric_split_convert", lnr, node_index)
}


#' Check if a node of a tree applies a ordinal split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.is_ordinal_split}{\code{IAI.is_ordinal_split}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_ordinal_split(lnr, 1)}
#'
#' @export
is_ordinal_split <- function(lnr, node_index) {
  jl_func("IAI.is_ordinal_split_convert", lnr, node_index)
}


#' Check if a node of a tree applies a mixed parallel/categoric split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.is_mixed_parallel_split}{\code{IAI.is_mixed_parallel_split}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_mixed_parallel_split(lnr, 1)}
#'
#' @export
is_mixed_parallel_split <- function(lnr, node_index) {
  jl_func("IAI.is_mixed_parallel_split_convert", lnr, node_index)
}


#' Check if a node of a tree applies a mixed ordinal/categoric split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.is_mixed_ordinal_split}{\code{IAI.is_mixed_ordinal_split}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_mixed_ordinal_split(lnr, 1)}
#'
#' @export
is_mixed_ordinal_split <- function(lnr, node_index) {
  jl_func("IAI.is_mixed_ordinal_split_convert", lnr, node_index)
}


#' Check if points with missing values go to the lower child at a split node of
#' of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.missing_goes_lower}{\code{IAI.missing_goes_lower}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::missing_goes_lower(lnr, 1)}
#'
#' @export
missing_goes_lower <- function(lnr, node_index) {
  jl_func("IAI.missing_goes_lower_convert", lnr, node_index)
}


#' Return the feature used in the split at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_split_feature}{\code{IAI.get_split_feature}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_split_feature(lnr, 1)}
#'
#' @export
get_split_feature <- function(lnr, node_index) {
  jl_func("IAI.get_split_feature_convert", lnr, node_index)
}


#' Return the threshold used in the split at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_split_threshold}{\code{IAI.get_split_threshold}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_split_threshold(lnr, 1)}
#'
#' @export
get_split_threshold <- function(lnr, node_index) {
  jl_func("IAI.get_split_threshold_convert", lnr, node_index)
}


#' Return the categoric/ordinal information used in the split at a node of a
#' tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_split_categories}{\code{IAI.get_split_categories}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_split_categories(lnr, 1)}
#'
#' @export
get_split_categories <- function(lnr, node_index) {
  jl_func("IAI.get_split_categories_convert", lnr, node_index)
}


#' Return the weights for numeric and categoric features used in the hyperplane
#' split at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_split_weights}{\code{IAI.get_split_weights}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_split_weights(lnr, 1)}
#'
#' @export
get_split_weights <- function(lnr, node_index) {
  out <- jl_func("IAI.get_split_weights_convert", lnr, node_index)
  names(out) <- c("numeric", "categoric")
  out
}


#' Return the predicted label at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_classification_label}{\code{IAI.get_classification_label}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_classification_label(lnr, 1)}
#'
#' @export
get_classification_label <- function(lnr, node_index, ...) {
  jl_func("IAI.get_classification_label_convert", lnr, node_index, ...)
}


#' Return the predicted probabilities of class membership at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_classification_proba}{\code{IAI.get_classification_proba}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_classification_proba(lnr, 1)}
#'
#' @export
get_classification_proba <- function(lnr, node_index, ...) {
  jl_func("IAI.get_classification_proba_convert", lnr, node_index, ...)
}


#' Return the constant term in the logistic regression prediction at a node of a
#' classification tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_constant-Tuple\%7BClassificationTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_regression_constant}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_constant(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
get_regression_constant.classification_tree_learner <- function(obj, node_index,
                                                                ...) {
  requires_iai_version("3.0.0", "get_regression_constant",
                       "with `classification_tree_learner`")
  get_regression_constant_common(obj, node_index, ...)
}
#' Return the constant term in the linear regression prediction at a node of a
#' regression tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_constant-Tuple\%7BRegressionTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_regression_constant}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_constant(lnr, 1)}
#'
#' @export
get_regression_constant.regression_tree_learner <- function(obj, node_index,
                                                            ...) {
  get_regression_constant_common(obj, node_index, ...)
}
#' Return the constant term in the cox regression prediction at a node of a
#' survival tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_constant-Tuple\%7BSurvivalTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_regression_constant}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_constant(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
get_regression_constant.survival_tree_learner <- function(obj, node_index,
                                                          ...) {
  requires_iai_version("3.0.0", "get_regression_constant",
                       "with `survival_tree_learner`")
  get_regression_constant_common(obj, node_index, ...)
}
#' Return the constant term in the linear regression prediction at a node of a
#' prescription tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_constant-Tuple\%7BPrescriptionTreeLearner\%2C\%20Int64\%2C\%20Any\%7D}{\code{IAI.get_regression_constant}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param treatment The treatment to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_constant(lnr, 1, "A")}
#'
#' @export
get_regression_constant.prescription_tree_learner <- function(obj, node_index,
                                                              treatment, ...) {
  get_regression_constant_common(obj, node_index, treatment, ...)
}


#' Return the weights for each feature in the logistic regression prediction at
#' a node of a classification tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_weights-Tuple\%7BClassificationTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_regression_weights}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_weights(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
get_regression_weights.classification_tree_learner <- function(obj, node_index,
                                                               ...) {
  requires_iai_version("3.0.0", "get_regression_weights",
                       "with `classification_tree_learner`")
  get_regression_weights_common(obj, node_index, ...)
}
#' Return the weights for each feature in the linear regression prediction at a
#' node of a regression tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_weights-Tuple\%7BRegressionTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_regression_weights}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_weights(lnr, 1)}
#'
#' @export
get_regression_weights.regression_tree_learner <- function(obj, node_index,
                                                           ...) {
  get_regression_weights_common(obj, node_index, ...)
}
#' Return the weights for each feature in the cox regression prediction at a
#' node of a survival tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_weights-Tuple\%7BSurvivalTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_regression_weights}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_weights(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
get_regression_weights.survival_tree_learner <- function(obj, node_index, ...) {
  requires_iai_version("3.0.0", "get_regression_weights",
                       "with `survival_tree_learner`")
  get_regression_weights_common(obj, node_index, ...)
}
#' Return the weights for each feature in the linear regression prediction at a
#' node of a prescription tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_regression_weights-Tuple\%7BPrescriptionTreeLearner\%2C\%20Int64\%2C\%20Any\%7D}{\code{IAI.get_regression_weights}}
#'
#' @param obj The learner to query.
#' @param node_index The node in the tree to query.
#' @param treatment The treatment to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{iai::get_regression_weights(lnr, 1, "A")}
#'
#' @export
get_regression_weights.prescription_tree_learner <- function(obj, node_index,
                                                             treatment, ...) {
  get_regression_weights_common(obj, node_index, treatment, ...)
}


#' Return the survival curve at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_survival_curve}{\code{IAI.get_survival_curve}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_survival_curve(lnr, 1)}
#'
#' @export
get_survival_curve <- function(lnr, node_index, ...) {
  set_obj_class(jl_func("IAI.get_survival_curve_convert", lnr, node_index, ...))
}


#' Return the predicted expected survival time at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_survival_expected_time}{\code{IAI.get_survival_expected_time}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_survival_expected_time(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_survival_expected_time <- function(lnr, node_index, ...) {
  requires_iai_version("2.1.0", "get_survival_expected_time")
  jl_func("IAI.get_survival_expected_time_convert", lnr, node_index, ...)
}


#' Return the predicted hazard ratio at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_survival_hazard}{\code{IAI.get_survival_hazard}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_survival_hazard(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_survival_hazard <- function(lnr, node_index, ...) {
  requires_iai_version("2.1.0", "get_survival_hazard")
  jl_func("IAI.get_survival_hazard_convert", lnr, node_index, ...)
}


#' Return the treatments ordered from most effective to least effective at a
#' node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_prescription_treatment_rank}{\code{IAI.get_prescription_treatment_rank}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_prescription_treatment_rank(lnr, 1)}
#'
#' @export
get_prescription_treatment_rank <- function(lnr, node_index, ...) {
  jl_func("IAI.get_prescription_treatment_rank_convert", lnr, node_index, ...)
}


#' Return the treatments ordered from most effective to least effective at a
#' node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_policy_treatment_rank}{\code{IAI.get_policy_treatment_rank}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_policy_treatment_rank(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
get_policy_treatment_rank <- function(lnr, node_index, ...) {
  requires_iai_version("2.0.0", "get_policy_treatment_rank")
  jl_func("IAI.get_policy_treatment_rank_convert", lnr, node_index, ...)
}


#' Return the quality of the treatments at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_policy_treatment_outcome}{\code{IAI.get_policy_treatment_outcome}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_policy_treatment_outcome(lnr, 1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_policy_treatment_outcome <- function(lnr, node_index, ...) {
  requires_iai_version("2.1.0", "get_policy_treatment_outcome")
  jl_func("IAI.get_policy_treatment_outcome_convert", lnr, node_index, ...)
}


#' Return the leaf index in a tree model into which each point in the features
#' falls
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.apply}{\code{IAI.apply}}
#'
#' @param lnr The learner or grid to query.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::apply(lnr, X)}
#'
#' @export
apply <- function(lnr, X) {
  jl_func("IAI.apply_convert", lnr, X)
}

#' Return the indices of the points in the features that fall into each node of
#' a trained tree model
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.apply_nodes}{\code{IAI.apply_nodes}}
#'
#' @param lnr The learner or grid to query.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::apply_nodes(lnr, X)}
#'
#' @export
apply_nodes <- function(lnr, X) {
  jl_func("IAI.apply_nodes_convert", lnr, X)
}


#' Return a matrix where entry \code{(i, j)} is true if the \code{i}th point in
#' the features passes through the \code{j}th node in a trained tree model.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.decision_path}{\code{IAI.decision_path}}
#'
#' @param lnr The learner or grid to query.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::decision_path(lnr, X)}
#'
#' @export
decision_path <- function(lnr, X) {
  jl_func("IAI.decision_path_convert", lnr, X)
}


#' Print the decision path through the learner for each sample in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.print_path}{\code{IAI.print_path}}
#'
#' @param lnr The learner or grid to query.
#' @param X The features of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples
#' \dontrun{
#' iai::print_path(lnr, X)
#' iai::print_path(lnr, X, 1)
#' }
#'
#' @export
print_path <- function(lnr, X, ...) {
  jl_func("IAI.print_path_convert", lnr, X, ...)
}


#' Generate a ranking of the variables in a tree learner according to their
#' importance during training. The results are normalized so that they sum to
#' one.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.variable_importance-Tuple\%7BTreeLearner\%7D}{\code{IAI.variable_importance}}
#'
#' @param obj The learner to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::variable_importance(lnr, ...)}
#'
#' @export
variable_importance.tree_learner <- function(obj, ...) {
  variable_importance_common(obj, ...)
}


#' For a binary classification problem, update the the predicted labels in the
#' leaves of the learner to predict a label only if the predicted probability is
#' at least the specified threshold.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.set_threshold!}{\code{IAI.set_threshold!}}
#'
#' @param lnr The learner to modify.
#' @param label The referenced label.
#' @param threshold The probability threshold above which \code{label} will be
#'                  be predicted.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::set_threshold(lnr, "A", 0.4)}
#'
#' @export
set_threshold <- function(lnr, label, threshold, ...) {
  jl_func("IAI.set_threshold_convert", lnr, label, threshold, ...)
  lnr
}


#' Output a learner as a PNG image
#'
#' Before using this function, either run \code{\link{load_graphviz}} or ensure
#' that \href{http://graphviz.org}{Graphviz} is installed and on the system
#' \code{PATH}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.write_png}{\code{IAI.write_png}}
#'
#' @param filename Where to save the output.
#' @param lnr The learner to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_png(file.path(tempdir(), "tree.png"), lnr)}
#'
#' @export
write_png <- function(filename, lnr, ...) {
  jl_func("IAI.write_png_convert", filename, lnr, ...) # nocov
}


#' Output a learner as a PDF image
#'
#' Before using this function, either run \code{\link{load_graphviz}} or ensure
#' that \href{http://graphviz.org}{Graphviz} is installed and on the system
#' \code{PATH}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.write_pdf}{\code{IAI.write_pdf}}
#'
#' @param filename Where to save the output.
#' @param lnr The learner to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_pdf(file.path(tempdir(), "tree.pdf"), lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
write_pdf <- function(filename, lnr, ...) { # nocov start
  requires_iai_version("2.1.0", "write_pdf")
  jl_func("IAI.write_pdf_convert", filename, lnr, ...)
} # nocov end


#' Output a learner as a SVG image
#'
#' Before using this function, either run \code{\link{load_graphviz}} or ensure
#' that \href{http://graphviz.org}{Graphviz} is installed and on the system
#' \code{PATH}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.write_svg}{\code{IAI.write_svg}}
#'
#' @param filename Where to save the output.
#' @param lnr The learner to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_svg(file.path(tempdir(), "tree.svg"), lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
write_svg <- function(filename, lnr, ...) { # nocov start
  requires_iai_version("2.1.0", "write_svg")
  jl_func("IAI.write_svg_convert", filename, lnr, ...)
} # nocov end


#' Output a learner in
#' \href{http://www.graphviz.org/content/dot-language/}{.dot format}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.write_dot}{\code{IAI.write_dot}}
#'
#' @param filename Where to save the output.
#' @param lnr The learner to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_dot(file.path(tempdir(), "tree.dot"), lnr)}
#'
#' @export
write_dot <- function(filename, lnr, ...) {
  jl_func("IAI.write_dot_convert", filename, lnr, ...)
}


#' Output a tree learner as an interactive browser visualization in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.write_html-Tuple\%7BAny\%2C\%20TreeLearner\%7D}{\code{IAI.write_html}}
#'
#' @param filename Where to save the output.
#' @param obj The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_html(file.path(tempdir(), "tree.html"), lnr)}
#'
#' @section IAI Compatibility:
#' Outputting a grid search requires IAI version 2.0 or higher.
#'
#' @export
write_html.tree_learner <- function(filename, obj, ...) {
  if ("grid_search" %in% class(obj)) {
    requires_iai_version("2.0.0", "write_html", "with `grid_search`")
  }
  write_html_common(filename, obj, ...)
}


#' Show interactive tree visualization of a tree learner in the default browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.show_in_browser-Tuple\%7BTreeLearner\%7D}{\code{IAI.show_in_browser}}
#'
#' @param obj The learner or grid to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_in_browser(lnr)}
#'
#' @section IAI Compatibility:
#' Showing a grid search requires IAI version 2.0 or higher.
#'
#' @export
show_in_browser.tree_learner <- function(obj, ...) {
  if ("grid_search" %in% class(obj)) {
    requires_iai_version("2.0.0", "show_in_browser", "with `grid_search`")
  }
  show_in_browser_common(obj, ...) # nocov
}


#' Output a tree learner as an interactive questionnaire in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.write_questionnaire-Tuple\%7BAny\%2C\%20TreeLearner\%7D}{\code{IAI.write_questionnaire}}
#'
#' @param filename Where to save the output.
#' @param obj The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_questionnaire(file.path(tempdir(), "questionnaire.html"), lnr)}
#'
#' @section IAI Compatibility:
#' Outputting a grid search requires IAI version 2.0 or higher.
#'
#' @export
write_questionnaire.tree_learner <- function(filename, obj, ...) {
  if ("grid_search" %in% class(obj)) {
    requires_iai_version("2.0.0", "write_questionnaire", "with `grid_search`")
  }
  write_questionnaire_common(filename, obj, ...)
}


#' Show an interactive questionnaire based on a tree learner in default browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.show_questionnaire-Tuple\%7BTreeLearner\%7D}{\code{IAI.show_questionnaire}}
#'
#' @param obj The learner or grid to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_questionnaire(lnr)}
#'
#' @section IAI Compatibility:
#' Showing a grid search requires IAI version 2.0 or higher.
#'
#' @export
show_questionnaire.tree_learner <- function(obj, ...) {
  if ("grid_search" %in% class(obj)) {
    requires_iai_version("2.0.0", "show_questionnaire", "with `grid_search`")
  }
  show_questionnaire_common(obj, ...) # nocov
}


#' Show the probability of a specified label when visualizing a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.set_display_label!}{\code{IAI.set_display_label!}}
#'
#' @param lnr The learner to modify.
#' @param display_label The label for which to show probabilities.
#'
#' @examples \dontrun{iai::set_display_label(lnr, "A")}
#'
#' @export
set_display_label <- function(lnr, display_label) {
  jl_func("IAI.set_display_label_convert", lnr, display_label)
  lnr
}


#' Reset the predicted probability displayed to be that of the predicted label
#' when visualizing a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.reset_display_label!}{\code{IAI.reset_display_label!}}
#'
#' @param lnr The learner to modify.
#'
#' @examples \dontrun{iai::reset_display_label(lnr)}
#'
#' @export
reset_display_label <- function(lnr) {
  jl_func("IAI.reset_display_label_convert", lnr)
  lnr
}


#' Specify an interactive tree visualization of a tree learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.TreePlot-Tuple\%7BTreeLearner\%7D}{\code{IAI.TreePlot}}
#'
#' @param lnr The learner to visualize.
#' @param ... Refer to the \href{https://docs.interpretable.ai/v3.1.0/IAITrees/advanced/#Advanced-Visualization-1}{Julia documentation on advanced tree visualization} for available parameters.
#'
#' @examples \dontrun{iai::tree_plot(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
tree_plot <- function(lnr, ...) {
  requires_iai_version("1.1.0", "tree_plot")
  set_obj_class(jl_func("IAI.TreePlot_convert", lnr, ...))
}


#' Specify an interactive questionnaire of a tree learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.Questionnaire-Tuple\%7BTreeLearner\%7D}{\code{IAI.Questionnaire}}
#'
#' @param obj The learner to visualize.
#' @param ... Refer to the \href{https://docs.interpretable.ai/v3.1.0/IAITrees/advanced/#Advanced-Visualization-1}{Julia documentation} for available parameters.
#'
#' @examples \dontrun{iai::questionnaire(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
questionnaire.tree_learner <- function(obj, ...) {
  requires_iai_version("1.1.0", "questionnaire", "with `tree_learner`")
  questionnaire_common(obj, ...)
}


#' Construct an interactive tree visualization of multiple tree learners as
#' specified by questions
#'
#' Refer to the
#' \href{https://docs.interpretable.ai/v3.1.0/IAI-R/julia/#R-Interactive-Visualizations-1}{documentation on advanced tree visualization}
#' for more information.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.MultiTreePlot-Tuple\%7BPair\%7D}{\code{IAI.MultiTreePlot}}
#'
#' @param obj The questions to visualize. Refer to the \href{https://docs.interpretable.ai/v3.1.0/IAITrees/visualization/#multivis-1}{Julia documentation on multi-learner visualizations} for more information.
#' @param ... Additional arguments (unused)
#'
#' @examples
#' \dontrun{
#' iai::multi_tree_plot(list("Visualizing" = list(
#'    "first learner" = lnr1,
#'    "second learner" = lnr2
#' )))
#' }
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
multi_tree_plot.default <- function(obj, ...) {
  requires_iai_version("1.1.0", "multi_tree_plot", "with `list`")
  multi_tree_plot_common(obj, ...)
}


#' Construct an interactive tree visualization of multiple tree learners from
#' the results of a grid search
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.MultiTreePlot-Tuple\%7BGridSearch\%7D}{\code{IAI.MultiTreePlot}}
#'
#' @param obj The grid to visualize
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::multi_tree_plot(grid)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
multi_tree_plot.grid_search <- function(obj, ...) {
  requires_iai_version("2.0.0", "multi_tree_plot", "with `grid_search`")
  multi_tree_plot_common(obj, ...)
}


#' Calculate similarity between the final tree in a tree learner with all trees
#' in new tree learner using variable importance scores.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.variable_importance_similarity}{\code{IAI.variable_importance_similarity}}
#'
#' @param lnr The original learner
#' @param new_lnr The new learner
#' @param ... Additional arguments (refer to Julia documentation)
#'
#' @examples \dontrun{iai::variable_importance_similarity(lnr, new_lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
variable_importance_similarity <- function(lnr, new_lnr, ...) {
  requires_iai_version("2.2.0", "variable_importance_similarity")
  jl_func("IAI.variable_importance_similarity_convert", lnr, new_lnr, ...)
}


#' Return a copy of the learner that uses a specific tree rather than the tree
#' with the best training objective.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_tree}{\code{IAI.get_tree}}
#'
#' @param lnr The original learner
#' @param index The index of the tree to use
#'
#' @examples \dontrun{iai::get_tree(lnr, index)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_tree <- function(lnr, index) {
  requires_iai_version("2.2.0", "get_tree")
  set_obj_class(jl_func("IAI.get_tree_convert", lnr, index))
}


#' Conduct a similarity comparison between the final tree in a learner and
#' all trees in a new learner to consider the tradeoff between training
#' performance and similarity to the original tree
#'
#' Refer to the
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/stability/#Tree-Stability-1}{documentation on tree stability}
#' for more information.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.SimilarityComparison}{\code{IAI.SimilarityComparison}}
#'
#' @param lnr The original learner
#' @param new_lnr The new learner
#' @param deviations The deviation between the original tree and each tree in
#'                   the new learner
#'
#' @examples \dontrun{iai::similarity_comparison(lnr, new_lnr, deviations)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
similarity_comparison <- function(lnr, new_lnr, deviations) {
  requires_iai_version("2.2.0", "similarity_comparison")
  set_obj_class(jl_func("IAI.SimilarityComparison_convert", lnr, new_lnr,
                        deviations))
}


#' Extract the training objective value for each candidate tree in the
#' comparison, where a lower value indicates a better solution
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_train_errors}{\code{IAI.get_train_errors}}
#'
#' @param similarity The similarity comparison
#'
#' @examples \dontrun{iai::get_train_errors(similarity)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_train_errors <- function(similarity) {
  requires_iai_version("2.2.0", "get_train_errors")
  jl_func("IAI.get_train_errors_convert", similarity)
}


#' Construct a \href{https://ggplot2.tidyverse.org/reference/ggplot.html}{\code{ggplot2::ggplot}} object plotting the results of the similarity comparison
#'
#' @param x The similarity comparison to plot
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{ggplot2::autoplot(similarity)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#' @export
autoplot.similarity_comparison <- function(x, ...) {
  requires_iai_version("2.2.0", "ggplot2::autoplot.similarity_comparison")

  plot_data <- data.frame(
      deviations = x$deviations,
      train_errors = get_train_errors(x),
      type = "All trees"
  )

  plot_data$type[x$best_tree_index] <- "Selected tree"

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$deviations,
                                          y = .data$train_errors,
                                          color = .data$type)) +

    ggplot2::geom_point() +
    # Redraw selected tree last so it shows up on top
    ggplot2::geom_point(data = plot_data[x$best_tree_index, ]) +
    ggplot2::labs(x = "Deviation from original tree (lower is better)",
                  y = "Training objective value (lower is better)")
}


#' Plot a similarity comparison
#'
#' @param x The similarity comparison to plot
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{plot(similarity)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @importFrom graphics plot
#' @export
plot.similarity_comparison <- function(x, ...) {
  if (!iai_version_less_than("2.2.0")) {
    print(ggplot2::autoplot(x, ...))
  } else {
    NextMethod() # nocov
  }
}


#' Conduct a stability analysis of the trees in a tree learner
#'
#' Refer to the
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/stability/#Tree-Stability-1}{documentation on tree stability}
#' for more information.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.StabilityAnalysis}{\code{IAI.StabilityAnalysis}}
#'
#' @param lnr The original learner
#' @param ... Additional arguments (refer to Julia documentation)
#'
#' @examples \dontrun{iai::stability_analysis(lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
stability_analysis <- function(lnr, ...) {
  requires_iai_version("2.2.0", "stability_analysis")
  set_obj_class(jl_func("IAI.StabilityAnalysis_convert", lnr, ...))
}


#' Return the trained trees in order of increasing objective value,
#' along with their variable importance scores for each feature
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_stability_results}{\code{IAI.get_stability_results}}
#'
#' @param stability The stability analysis to query
#'
#' @examples \dontrun{iai::get_stability_results(stability)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_stability_results <- function(stability) {
  requires_iai_version("2.2.0", "get_stability_results")
  jl_func("IAI.get_stability_results_convert", stability)
}


#' Return the indices of the trees assigned to each cluster, under the
#' clustering of a given number of trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_cluster_assignments}{\code{IAI.get_cluster_assignments}}
#'
#' @param stability The stability analysis to query
#' @param num_trees The number of trees to include in the clustering
#'
#' @examples \dontrun{iai::get_cluster_assignments(stability, num_trees)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_cluster_assignments <- function(stability, num_trees) {
  requires_iai_version("2.2.0", "get_cluster_assignments")
  out <- jl_func("IAI.get_cluster_assignments_convert", stability, num_trees)
  as.list(out)
}


#' Return the centroid information for each cluster, under the clustering of a
#' given number of trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_cluster_details}{\code{IAI.get_cluster_details}}
#'
#' @param stability The stability analysis to query
#' @param num_trees The number of trees to include in the clustering
#'
#' @examples \dontrun{iai::get_cluster_details(stability, num_trees)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_cluster_details <- function(stability, num_trees) {
  requires_iai_version("2.2.0", "get_cluster_details")
  jl_func("IAI.get_cluster_details_convert", stability, num_trees)
}


#' Return the distances between the centroids of each pair of clusters, under
#' the clustering of a given number of trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/IAITrees/reference/#IAI.get_cluster_distances}{\code{IAI.get_cluster_distances}}
#'
#' @param stability The stability analysis to query
#' @param num_trees The number of trees to include in the clustering
#'
#' @examples \dontrun{iai::get_cluster_distances(stability, num_trees)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_cluster_distances <- function(stability, num_trees) {
  requires_iai_version("2.2.0", "get_cluster_distances")
  jl_func("IAI.get_cluster_distances_convert", stability, num_trees)
}


#' Construct a \href{https://ggplot2.tidyverse.org/reference/ggplot.html}{\code{ggplot2::ggplot}} object plotting the results of the stability analysis
#'
#' @param x The stability analysis to plot
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{ggplot2::autoplot(stability)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#' @export
autoplot.stability_analysis <- function(x, ...) {
  requires_iai_version("2.2.0", "ggplot2::autoplot.stability_analysis")

  plot_data <- data.frame(
      x = seq_along(x$train_errors),
      train_errors = x$train_errors,
      n_clusts = sapply(x$cluster_centers, ncol),
      cluster_r2 = x$cluster_r2
  )

  plot1 <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = .data$x, y = .data$train_errors)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = "Training objective")

  plot2 <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = .data$x, y = .data$n_clusts)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = "Number of clusters")

  plot3 <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = .data$x, y = .data$cluster_r2)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Trees (ordered by training objective)",
                  y = "Cluster R-squared")

  cowplot::plot_grid(plot1, plot2, plot3, ncol = 1)
}


#' Plot a stability analysis
#'
#' @param x The stability analysis to plot
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{plot(stability)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @importFrom graphics plot
#' @export
plot.stability_analysis <- function(x, ...) {
  if (!iai_version_less_than("2.2.0")) {
    print(ggplot2::autoplot(x, ...))
  } else {
    NextMethod() # nocov
  }
}
