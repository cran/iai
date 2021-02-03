#' Return the number of nodes in a trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_num_nodes}{\code{IAI.get_num_nodes}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.is_leaf}{\code{IAI.is_leaf}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_depth}{\code{IAI.get_depth}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_num_samples}{\code{IAI.get_num_samples}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_parent}{\code{IAI.get_parent}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_lower_child}{\code{IAI.get_lower_child}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_upper_child}{\code{IAI.get_upper_child}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.is_parallel_split}{\code{IAI.is_parallel_split}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.is_hyperplane_split}{\code{IAI.is_hyperplane_split}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.is_categoric_split}{\code{IAI.is_categoric_split}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.is_ordinal_split}{\code{IAI.is_ordinal_split}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.is_mixed_parallel_split}{\code{IAI.is_mixed_parallel_split}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.is_mixed_ordinal_split}{\code{IAI.is_mixed_ordinal_split}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.missing_goes_lower}{\code{IAI.missing_goes_lower}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_split_feature}{\code{IAI.get_split_feature}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_split_threshold}{\code{IAI.get_split_threshold}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_split_categories}{\code{IAI.get_split_categories}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_split_weights}{\code{IAI.get_split_weights}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_classification_label}{\code{IAI.get_classification_label}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_classification_proba}{\code{IAI.get_classification_proba}}
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


#' Return the constant term in the regression prediction at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAITrees-API-Reference-1}{\code{IAI.get_regression_constant}}
#' (for regression or prescription tree learners as appropriate)
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... If a prescription problem, the treatment to query.
#'
#' @examples
#' \dontrun{
#' iai::get_regression_constant(lnr, 1)
#' iai::get_regression_constant(lnr, 1, "A")
#' }
#'
#' @export
get_regression_constant <- function(lnr, node_index, ...) {
  jl_func("IAI.get_regression_constant_convert", lnr, node_index, ...)
}


#' Return the weights for each feature in the regression prediction at a node of
#' a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAITrees-API-Reference-1}{\code{IAI.get_regression_weights}}
#' (for regression or prescription tree learners as appropriate)
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... If a prescription problem, the treatment to query.
#'
#' @examples
#' \dontrun{
#' iai::get_regression_weights(lnr, 1)
#' iai::get_regression_weights(lnr, 1, "A")
#' }
#'
#' @export
get_regression_weights <- function(lnr, node_index, ...) {
  out <- jl_func("IAI.get_regression_weights_convert", lnr, node_index, ...)
  names(out) <- c("numeric", "categoric")
  out
}


#' Return the survival curve at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_survival_curve}{\code{IAI.get_survival_curve}}
#'
#' @param lnr The learner to query.
#' @param node_index The node in the tree to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::get_survival_curve(lnr, 1)}
#'
#' @export
get_survival_curve <- function(lnr, node_index, ...) {
  jl_func("IAI.get_survival_curve_convert", lnr, node_index, ...)
}


#' Return the predicted expected survival time at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_survival_expected_time}{\code{IAI.get_survival_expected_time}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_survival_hazard}{\code{IAI.get_survival_hazard}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_prescription_treatment_rank}{\code{IAI.get_prescription_treatment_rank}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_policy_treatment_rank}{\code{IAI.get_policy_treatment_rank}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.get_policy_treatment_outcome}{\code{IAI.get_policy_treatment_outcome}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.apply}{\code{IAI.apply}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.apply_nodes}{\code{IAI.apply_nodes}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.decision_path}{\code{IAI.decision_path}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.print_path}{\code{IAI.print_path}}
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


#' For a binary classification problem, update the the predicted labels in the
#' leaves of the learner to predict a label only if the predicted probability is
#' at least the specified threshold.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.set_threshold!}{\code{IAI.set_threshold!}}
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
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.write_png}{\code{IAI.write_png}}
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
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.write_pdf}{\code{IAI.write_pdf}}
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
write_pdf <- function(filename, lnr, ...) {
  requires_iai_version("2.1.0", "write_pdf")
  jl_func("IAI.write_pdf_convert", filename, lnr, ...) # nocov
}


#' Output a learner as a SVG image
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.write_svg}{\code{IAI.write_svg}}
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
write_svg <- function(filename, lnr, ...) {
  requires_iai_version("2.1.0", "write_svg")
  jl_func("IAI.write_svg_convert", filename, lnr, ...) # nocov
}


#' Output a learner in
#' \href{http://www.graphviz.org/content/dot-language/}{.dot format}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.write_dot}{\code{IAI.write_dot}}
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


#' Output a learner as an interactive browser visualization in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.write_html}{\code{IAI.write_html}}
#'
#' @param filename Where to save the output.
#' @param lnr The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_html(file.path(tempdir(), "tree.html"), lnr)}
#'
#' @section IAI Compatibility:
#' Outputting a grid search requires IAI version 2.0 or higher.
#'
#' @export
write_html <- function(filename, lnr, ...) {
  jl_func("IAI.write_html_convert", filename, lnr, ...)
}


#' Output a learner as an interactive questionnaire in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.write_questionnaire}{\code{IAI.write_questionnaire}}
#'
#' @param filename Where to save the output.
#' @param lnr The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_questionnaire(file.path(tempdir(), "questionnaire.html"), lnr)}
#'
#' @section IAI Compatibility:
#' Outputting a grid search requires IAI version 2.0 or higher.
#'
#' @export
write_questionnaire <- function(filename, lnr, ...) {
  jl_func("IAI.write_questionnaire_convert", filename, lnr, ...)
}


#' Show an interactive questionnaire based on a learner in default browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.show_questionnaire}{\code{IAI.show_questionnaire}}
#'
#' @param lnr The learner or grid to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_questionnaire(lnr)}
#'
#' @section IAI Compatibility:
#' Showing a grid search requires IAI version 2.0 or higher.
#'
#' @export
show_questionnaire <- function(lnr, ...) {
  jl_func("IAI.show_questionnaire_convert", lnr, ...) # nocov
}


#' Show the probability of a specified label when visualizing a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.set_display_label!}{\code{IAI.set_display_label!}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.reset_display_label!}{\code{IAI.reset_display_label!}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.TreePlot}{\code{IAI.TreePlot}}
#'
#' @param lnr The learner to visualize.
#' @param ... Refer to the \href{https://docs.interpretable.ai/v2.1.0/IAITrees/advanced/#Advanced-Visualization-1}{Julia documentation on advanced tree visualization} for available parameters.
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.Questionnaire}{\code{IAI.Questionnaire}}
#'
#' @param lnr The learner to visualize.
#' @param ... Refer to the \href{https://docs.interpretable.ai/v2.1.0/IAITrees/advanced/#Advanced-Visualization-1}{Julia documentation} for available parameters.
#'
#' @examples \dontrun{iai::questionnaire(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
questionnaire <- function(lnr, ...) {
  requires_iai_version("1.1.0", "questionnaire")
  set_obj_class(jl_func("IAI.Questionnaire_convert", lnr, ...))
}


#' Generic function for constructing an interactive tree visualization of
#' multiple tree learners
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.MultiTreePlot}{\code{IAI.MultiTreePlot}}
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
multi_tree_plot <- function (obj, ...) {
  UseMethod("multi_tree_plot", obj)
}


#' Construct an interactive tree visualization of multiple tree learners as
#' specified by questions
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.MultiTreePlot}{\code{IAI.MultiTreePlot}}
#'
#' @param obj The questions to visualize. Refer to the \href{https://docs.interpretable.ai/v2.1.0/IAITrees/visualization/#multivis-1}{Julia documentation on multi-learner visualizations} for more information.
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
  requires_iai_version("1.1.0", "multi_tree_plot")
  set_obj_class(jl_func("IAI.MultiTreePlot_convert", obj))
}


#' Construct an interactive tree visualization of multiple tree learners from
#' the results of a grid search
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.MultiTreePlot}{\code{IAI.MultiTreePlot}}
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
  requires_iai_version("2.0.0", "multi_tree_plot")
  set_obj_class(jl_func("IAI.MultiTreePlot_convert", obj))
}


#' Generic function for constructing an interactive questionnaire using
#' multiple tree learners
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.MultiQuestionnaire}{\code{IAI.MultiQuestionnaire}}
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
multi_questionnaire <- function (obj, ...) {
  UseMethod("multi_questionnaire", obj)
}


#' Construct an interactive questionnaire using multiple tree learners as
#' specified by questions
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.MultiQuestionnaire}{\code{IAI.MultiQuestionnaire}}
#'
#' @param obj The questions to visualize. Refer to the \href{https://docs.interpretable.ai/v2.1.0/IAITrees/visualization/#multivis-1}{Julia documentation on multi-learner visualizations} for more information.
#' @param ... Additional arguments (unused)
#'
#' @examples
#' \dontrun{
#' iai::multi_questionnaire(list("Questionnaire for" = list(
#'    "first learner" = lnr1,
#'    "second learner" = lnr2
#' )))
#' }
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
multi_questionnaire.default <- function(obj, ...) {
  requires_iai_version("1.1.0", "multi_questionnaire")
  set_obj_class(jl_func("IAI.MultiQuestionnaire_convert", obj))
}


#' Construct an interactive tree questionnaire using multiple tree learners from
#' the results of a grid search
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAITrees/reference/#IAI.MultiQuestionnaire}{\code{IAI.MultiQuestionnaire}}
#'
#' @param obj The grid to visualize
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::multi_questionnaire(grid)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
multi_questionnaire.grid_search <- function(obj, ...) {
  requires_iai_version("2.0.0", "multi_questionnaire")
  set_obj_class(jl_func("IAI.MultiTreePlot_convert", obj))
}
