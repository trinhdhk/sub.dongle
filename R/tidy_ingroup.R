#' Within-group effect by rebasing the group variable
#' @description
#' This function rebase the group variable to different levels and refit the model, then tidy it.
#' The aim is to get the heterogenous effect of other variables that have interaction with the group variable.
#' @details Different from \code{tidy_subgroup}, this function does not fit on the subcohort,
#' hence does not assume interaction between group variable and all other covariables in the model.
#' If one covariable does not have interaction with the group variable, the estimated effect would be unchanged.
#' @author Trinh Dong
#' @param model The template model, will determine what method to use
#' @param group A vector of grouping variables
#' @param data Dataset for the performing subgroup. If missing, \code{base_model$data} will be inferred.
#' If the model object does not contains data, an error will be thrown.
#' @param conf.int [\code{TRUE}] include confidence interval
#' @param ... additional parameters passed to the method
#' @param .progress [\code{FALSE}] Print progress?
#' @return a tibble of class tidy_subgroup_tbl
#' @seealso [generics::tidy()] [future::future()] [future.apply::future_lapply()] [tidy_ingroup()]
#'
#' @export

tidy_ingroup <- function(
    model,
    group,
    data,
    conf.int = TRUE,
    ...,
    .progress = FALSE
){
  if (missing(data)) {
    data <- model.frame(model)
  }
  if (is.null(data)) {
    cli::cli_abort(
      c(
        'Failed to infer dataset from the model. The model may not have saved the data.',
        i = 'When `data` is missing, function infers data from model.frame().
        You cand specify data in the argument.'
      )
    )
  }
  group <- rlang::enexpr(group)
  group <- ._get_group_(group, data)

  # Assert if group has interaction
  all_ia <- ._get_interaction_terms_(model)
  ._assert_ia_(names(group), all_ia$ia.vars)

  # Do the work
  progress_handler <- if (isFALSE(.progress)) progressr::handler_void() else
    if (isTRUE(.progress)) progressr::handler_cli() else .progress
  progressr::with_progress({
    pr <- progressr::progressor(along = group)
    ingroup_fit <- future.apply::future_lapply(
      seq_along(group), \(i) {
        pr(message = glue::glue('Reparametrising {names(group[i])}'))
        ._tidy_ingroup_(model, group[i], all_ia, data, conf.int = conf.int, .keep_all = i==1, ...)
      }, future.seed = TRUE)}, handlers = progress_handler)

  ingroup_fit <- dplyr::bind_rows(ingroup_fit)
  ._append_class_(ingroup_fit, 'tidy_subgroup_tbl')
}

._get_interaction_terms_ <- function(fit){
  fml <- formula(fit)
  ia.terms <- attr(terms(fml),"term.labels")[attr(terms(fml),"order")>1]
  ia.factors <- attr(terms(fml),"factors")[,ia.terms, drop = FALSE]
  ia.vars <- sapply(ia.terms,
                    function(ia.term) rownames(ia.factors)[ia.factors[,ia.term] == 1],
                    simplify = FALSE)
  return(list(ia.terms = ia.terms, ia.vars = ia.vars))
}

._assert_ia_ <- function(vars, ia_vars){
  for (v in vars){
    if (!v %in% unique(unlist(ia_vars)))
      cli::cli_abort(c(
        paste(deparse(substitute(v)), 'not exists in interaction terms.'),
        i = "Either this is a bug, or this variable does not have any interaction with other variables.",
        v = "Specify interaction or use tidy_subgroup to refit on subgroup."
      ))
  }
}

._tidy_ingroup_ <-
  function(model, grp, ia, data, conf.int, .keep_all,...){

    group_var <- names(grp)
    group_lv <- grp[[1]]
    has_base <- sapply(ia$ia.vars, function(ia_var) group_var %in% ia_var)
    base_ia <- sapply(ia, `[`, has_base, simplify = FALSE)
    summary_vars <-
      unname(unlist(lapply(base_ia$ia.vars,
                           function(ia.var) ia.var[ia.var != group_var])))

    group_var_name <- if (!is.null(attr(data[[group_var]], 'label'))) attr(data[[group_var]], 'label') else group_var
    data[[group_var]] <- factor(data[[group_var]], ordered = FALSE)
    group_fit <- lapply(
      group_lv,
      \(lv) {
        data[[group_var]] <- relevel(data[[group_var]], ref = lv)
        refit <- update(model, data=data)
        tidy(refit, conf.int = conf.int, ...) |>
          dplyr::mutate(.subgroup_val = lv)
      }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(.subgroup_name = group_var_name ,
                    .subgroup_label = paste(group_var_name, .subgroup_val, sep= ' = ')) |>
      dplyr::select(.subgroup_label, .subgroup_name, .subgroup_val, dplyr::everything())
  }
