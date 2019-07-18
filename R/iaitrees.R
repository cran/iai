#' get_num_nodes
#'
#' Return the number of nodes in a trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_num_nodes}{\code{IAI.get_num_nodes}}
#'
#' @usage get_num_nodes(lnr)
#'
#' @param lnr The learner or grid to query.
#'
#' @examples \dontrun{iai::get_num_nodes(lnr)}
#'
#' @export
get_num_nodes <- function(lnr) {
  jl_func("IAI.get_num_nodes_convert", lnr)
}


#' is_leaf
#'
#' Check if a node of a tree is a leaf
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_leaf}{\code{IAI.is_leaf}}
#'
#' @usage is_leaf(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_leaf(lnr, 1)}
#'
#' @export
is_leaf <- function(lnr, node_index) {
  jl_func("IAI.is_leaf_convert", lnr, node_index)
}


#' get_depth
#'
#' Get the depth of a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_depth}{\code{IAI.get_depth}}
#'
#' @usage get_depth(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_depth(lnr, 1)}
#'
#' @export
get_depth <- function(lnr, node_index) {
  jl_func("IAI.get_depth_convert", lnr, node_index)
}


#' get_num_samples
#'
#' Get the number of training points contained in a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_num_samples}{\code{IAI.get_num_samples}}
#'
#' @usage get_num_samples(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_num_samples(lnr, 1)}
#'
#' @export
get_num_samples <- function(lnr, node_index) {
  jl_func("IAI.get_num_samples_convert", lnr, node_index)
}


#' get_parent
#'
#' Get the index of the parent node at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_parent}{\code{IAI.get_parent}}
#'
#' @usage get_parent(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_parent(lnr, 2)}
#'
#' @export
get_parent <- function(lnr, node_index) {
  jl_func("IAI.get_parent_convert", lnr, node_index)
}


#' get_lower_child
#'
#' Get the index of the lower child at a split node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_lower_child}{\code{IAI.get_lower_child}}
#'
#' @usage get_lower_child(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_lower_child(lnr, 1)}
#'
#' @export
get_lower_child <- function(lnr, node_index) {
  jl_func("IAI.get_lower_child_convert", lnr, node_index)
}


#' get_upper_child
#'
#' Get the index of the upper child at a split node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_upper_child}{\code{IAI.get_upper_child}}
#'
#' @usage get_upper_child(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_upper_child(lnr, 1)}
#'
#' @export
get_upper_child <- function(lnr, node_index) {
  jl_func("IAI.get_upper_child_convert", lnr, node_index)
}


#' is_parallel_split
#'
#' Check if a node of a tree applies a parallel split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_parallel_split}{\code{IAI.is_parallel_split}}
#'
#' @usage is_parallel_split(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_parallel_split(lnr, 1)}
#'
#' @export
is_parallel_split <- function(lnr, node_index) {
  jl_func("IAI.is_parallel_split_convert", lnr, node_index)
}


#' is_hyperplane_split
#'
#' Check if a node of a tree applies a hyperplane split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_hyperplane_split}{\code{IAI.is_hyperplane_split}}
#'
#' @usage is_hyperplane_split(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_hyperplane_split(lnr, 1)}
#'
#' @export
is_hyperplane_split <- function(lnr, node_index) {
  jl_func("IAI.is_hyperplane_split_convert", lnr, node_index)
}


#' is_categoric_split
#'
#' Check if a node of a tree applies a categoric split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_categoric_split}{\code{IAI.is_categoric_split}}
#'
#' @usage is_categoric_split(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_categoric_split(lnr, 1)}
#'
#' @export
is_categoric_split <- function(lnr, node_index) {
  jl_func("IAI.is_categoric_split_convert", lnr, node_index)
}


#' is_ordinal_split
#'
#' Check if a node of a tree applies a ordinal split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_ordinal_split}{\code{IAI.is_ordinal_split}}
#'
#' @usage is_ordinal_split(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_ordinal_split(lnr, 1)}
#'
#' @export
is_ordinal_split <- function(lnr, node_index) {
  jl_func("IAI.is_ordinal_split_convert", lnr, node_index)
}


#' is_mixed_parallel_split
#'
#' Check if a node of a tree applies a mixed parallel/categoric split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_mixed_parallel_split}{\code{IAI.is_mixed_parallel_split}}
#'
#' @usage is_mixed_parallel_split(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_mixed_parallel_split(lnr, 1)}
#'
#' @export
is_mixed_parallel_split <- function(lnr, node_index) {
  jl_func("IAI.is_mixed_parallel_split_convert", lnr, node_index)
}


#' is_mixed_ordinal_split
#'
#' Check if a node of a tree applies a mixed ordinal/categoric split
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_mixed_ordinal_split}{\code{IAI.is_mixed_ordinal_split}}
#'
#' @usage is_mixed_ordinal_split(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::is_mixed_ordinal_split(lnr, 1)}
#'
#' @export
is_mixed_ordinal_split <- function(lnr, node_index) {
  jl_func("IAI.is_mixed_ordinal_split_convert", lnr, node_index)
}


#' missing_goes_lower
#'
#' Check if points with missing values go to the lower child at a split node of
#' of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.missing_goes_lower}{\code{IAI.missing_goes_lower}}
#'
#' @usage missing_goes_lower(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::missing_goes_lower(lnr, 1)}
#'
#' @export
missing_goes_lower <- function(lnr, node_index) {
  jl_func("IAI.missing_goes_lower_convert", lnr, node_index)
}


#' get_split_feature
#'
#' Return the feature used in the split at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_split_feature}{\code{IAI.get_split_feature}}
#'
#' @usage get_split_feature(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_split_feature(lnr, 1)}
#'
#' @export
get_split_feature <- function(lnr, node_index) {
  jl_func("IAI.get_split_feature_convert", lnr, node_index)
}


#' get_split_threshold
#'
#' Return the threshold used in the split at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_split_threshold}{\code{IAI.get_split_threshold}}
#'
#' @usage get_split_threshold(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_split_threshold(lnr, 1)}
#'
#' @export
get_split_threshold <- function(lnr, node_index) {
  jl_func("IAI.get_split_threshold_convert", lnr, node_index)
}


#' get_split_categories
#'
#' Return the categoric/ordinal information used in the split at a node of a
#' tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_split_categories}{\code{IAI.get_split_categories}}
#'
#' @usage get_split_categories(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_split_categories(lnr, 1)}
#'
#' @export
get_split_categories <- function(lnr, node_index) {
  jl_func("IAI.get_split_categories_convert", lnr, node_index)
}


#' get_split_weights
#'
#' Return the weights for numeric and categoric features used in the hyperplane
#' split at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_split_weights}{\code{IAI.get_split_weights}}
#'
#' @usage get_split_weights(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
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


#' get_classification_label
#'
#' Return the predicted label at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_classification_label}{\code{IAI.get_classification_label}}
#'
#' @usage get_classification_label(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_classification_label(lnr, 1)}
#'
#' @export
get_classification_label <- function(lnr, node_index) {
  jl_func("IAI.get_classification_label_convert", lnr, node_index)
}


#' get_classification_proba
#'
#' Return the predicted probabilities of class membership at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_classification_proba}{\code{IAI.get_classification_proba}}
#'
#' @usage get_classification_proba(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_classification_proba(lnr, 1)}
#'
#' @export
get_classification_proba <- function(lnr, node_index) {
  jl_func("IAI.get_classification_proba_convert", lnr, node_index)
}


#' get_regression_constant
#'
#' Return the constant term in the regression prediction at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/}{\code{IAI.get_regression_constant}}
#' (for regression or prescription tree learners as appropriate)
#'
#' @usage get_regression_constant(lnr, node_index, ...)
#'
#' @param lnr The learner or grid to query.
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


#' get_regression_weights
#'
#' Return the weights for each feature in the regression prediction at a node of
#' a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/}{\code{IAI.get_regression_weights}}
#' (for regression or prescription tree learners as appropriate)
#'
#' @usage get_regression_weights(lnr, node_index, ...)
#'
#' @param lnr The learner or grid to query.
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


#' get_survival_curve
#'
#' Return the survival curve at a node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_survival_curve}{\code{IAI.get_survival_curve}}
#'
#' @usage get_survival_curve(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_survival_curve(lnr, 1)}
#'
#' @export
get_survival_curve <- function(lnr, node_index) {
  jl_func("IAI.get_survival_curve_convert", lnr, node_index)
}


#' get_prescription_treatment_rank
#'
#' Return the treatments ordered from most effective to least effective at a
#' node of a tree
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.get_prescription_treatment_rank}{\code{IAI.get_prescription_treatment_rank}}
#'
#' @usage get_prescription_treatment_rank(lnr, node_index)
#'
#' @param lnr The learner or grid to query.
#' @param node_index The node in the tree to query.
#'
#' @examples \dontrun{iai::get_prescription_treatment_rank(lnr, 1)}
#'
#' @export
get_prescription_treatment_rank <- function(lnr, node_index) {
  jl_func("IAI.get_prescription_treatment_rank_convert", lnr, node_index)
}


#' apply
#'
#' Return the leaf index in a tree model into which each point in the features
#' falls
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.apply}{\code{IAI.apply}}
#'
#' @usage apply(lnr, X)
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

#' apply_nodes
#'
#' Return the indices of the points in the features that fall into each node of
#' a trained tree model
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.apply_nodes}{\code{IAI.apply_nodes}}
#'
#' @usage apply_nodes(lnr, X)
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


#' decision_path
#'
#' Return a matrix where entry \code{(i, j)} is true if the \code{i}th point in
#' the features passes through the \code{j}th node in a trained tree model.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.decision_path}{\code{IAI.decision_path}}
#'
#' @usage decision_path(lnr, X)
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


#' print_path
#'
#' Print the decision path through the learner for each sample in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.print_path}{\code{IAI.print_path}}
#'
#' @usage print_path(lnr, X, ...)
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


#' variable_importance
#'
#' Generate a ranking of the variables in the learner according to their
#' importance when training the trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.variable_importance}{\code{IAI.variable_importance}}
#'
#' @usage variable_importance(lnr)
#'
#' @param lnr The learner or grid to query.
#'
#' @examples \dontrun{iai::variable_importance(lnr)}
#'
#' @export
variable_importance <- function(lnr) {
  jl_func("IAI.variable_importance_convert", lnr)
}


#' set_threshold
#'
#' For a binary classification problem, update the the predicted labels in the
#' leaves of the learner to predict a label only if the predicted probability is
#' at least the specified threshold.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.set_threshold!}{\code{IAI.set_threshold!}}
#'
#' @usage set_threshold(lnr, label, threshold, ...)
#'
#' @param lnr The learner or grid to modify.
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
}


#' write_png
#'
#' Output a learner as a PNG image
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.write_png}{\code{IAI.write_png}}
#'
#' @usage write_png(filename, lnr, ...)
#'
#' @param filename Where to save the output.
#' @param lnr The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_png(file.path(tempdir(), "tree.png"), lnr)}
#'
#' @export
write_png <- function(filename, lnr, ...) {
  jl_func("IAI.write_png_convert", filename, lnr, ...) # nocov
}


#' write_dot
#'
#' Output a learner in
#' \href{http://www.graphviz.org/content/dot-language/}{.dot format}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.write_dot}{\code{IAI.write_dot}}
#'
#' @usage write_dot(filename, lnr, ...)
#'
#' @param filename Where to save the output.
#' @param lnr The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_dot(file.path(tempdir(), "tree.dot"), lnr)}
#'
#' @export
write_dot <- function(filename, lnr, ...) {
  jl_func("IAI.write_dot_convert", filename, lnr, ...)
}


#' write_html
#'
#' Output a learner as an interactive browser visualization in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.write_html}{\code{IAI.write_html}}
#'
#' @usage write_html(filename, lnr, ...)
#'
#' @param filename Where to save the output.
#' @param lnr The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_html(file.path(tempdir(), "tree.html"), lnr)}
#'
#' @export
write_html <- function(filename, lnr, ...) {
  jl_func("IAI.write_html_convert", filename, lnr, ...)
}


#' write_questionnaire
#'
#' Output a learner as an interactive questionnaire in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.write_questionnaire}{\code{IAI.write_questionnaire}}
#'
#' @usage write_questionnaire(filename, lnr, ...)
#'
#' @param filename Where to save the output.
#' @param lnr The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_questionnaire(file.path(tempdir(), "questionnaire.html"), lnr)}
#'
#' @export
write_questionnaire <- function(filename, lnr, ...) {
  jl_func("IAI.write_questionnaire_convert", filename, lnr, ...)
}


#' show_questionnaire
#'
#' Show an interactive questionnaire based on a learner in default browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.show_questionnaire}{\code{IAI.show_questionnaire}}
#'
#' @usage show_questionnaire(lnr, ...)
#'
#' @param lnr The learner to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_questionnaire(lnr)}
#'
#' @export
show_questionnaire <- function(lnr, ...) {
  jl_func("IAI.show_questionnaire_convert", lnr, ...) # nocov
}


#' set_display_label
#'
#' Show the probability of a specified label when visualizing a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.set_display_label!}{\code{IAI.set_display_label!}}
#'
#' @usage set_display_label(lnr, display_label)
#'
#' @param lnr The learner to modify.
#' @param display_label The label for which to show probabilities.
#'
#' @examples \dontrun{iai::set_display_label(lnr, "A")}
#'
#' @export
set_display_label <- function(lnr, display_label) {
  jl_func("IAI.set_display_label_convert", lnr, display_label)
}


#' reset_display_label
#'
#' Reset the predicted probability displayed to be that of the predicted label
#' when visualizing a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.reset_display_label!}{\code{IAI.reset_display_label!}}
#'
#' @usage reset_display_label(lnr)
#'
#' @param lnr The learner to modify.
#'
#' @examples \dontrun{iai::reset_display_label(lnr)}
#'
#' @export
reset_display_label <- function(lnr) {
  jl_func("IAI.reset_display_label_convert", lnr)
}
