#' Refit model on sub-cohort
#' @description This function refits the model on sub-cohort, then tidy it.
#' @details
#' This function relies on \code{tidy} methods. Therefore, as long as there is a suitable method for tidying the fit, this function should work.
#' @author Trinh Dong
#' @param base_model The main model, will determine that method to use
#' @param group A vector of grouping variables
#' @param data Dataset for the performing subgroup. If missing, \code{base_model$data} will be inferred.
#' If the model object does not contains data, an error will be thrown.
#' @param conf.int [\code{TRUE}] include confidence interval
#' @param ... additional parameters passed to the method
#' @param .overall [\code{FALSE}] Save the overall model in the output?
#' @param .progress [\code{FALSE}] Print progress?
#' @return a tibble
#' @importFrom rlang enquo
#' @importFrom generics tidy
#' @importFrom dplyr bind_rows
#' @seealso [generics::tidy()] [future::future()] [future.apply::future_lapply()]
#' @export
tidy_subgroup <- function(
    base_model,
    group,
    data,
    conf.int = TRUE,
    ...,
    .overall = FALSE,
    .progress = FALSE){
    if (missing(data)) {
      data <- base_model$data
    }
    if (is.null(data)) {
      cli::cli_abort(
        c(
          'Failed to infer dataset from the model. The model may not have saved the data.',
          i = 'Specified data in the argument.'
        )
      )
    }
    overall_fit <- tidy(base_model, ...) |>
      dplyr::mutate(.subgroup_name = '(All patients)', .subgroup_val = NA, .subgroup_label = '(All patients)') |>
      dplyr::select(.subgroup_label, .subgroup_name, .subgroup_val, dplyr::everything())

    group <- enquo(group)
    group <- ._get_group_(group, data)
    if (.progress){
      progressr::with_progress({
        pr <- progressr::progressor(along = group)
        subgroup_fit <- future.apply::future_lapply(
          seq_along(group), \(i) {
            pr(message = glue::glue('Fitting {names(group[i])}'))
            ._tidy_subgroup_(base_model, group[i], data, ...)
          }, future.seed = TRUE)}, handlers = progressr::handler_cli())
    }
    subgroup_fit <- lapply(seq_along(group), \(i) ._tidy_subgroup_(base_model, group[i], data, conf.int = conf.int, ...))
    subgroup_fit <- dplyr::bind_rows(subgroup_fit)
    if (.overall) return(._append_class_(dplyr::bind_rows(overall_fit, subgroup_fit), 'tidy_subgroup_tbl'))
    ._append_class_(subgroup_fit, 'tidy_subgroup_tbl')
}

._append_class_ <- function(x, new_class) {
  class(x) <- c(new_class, class(x))
  x
}
._get_group_ <- function(group, data){
  # group <- rlang::enquo(group)
  group_var <- names(dplyr::select(data[0,], !!group))
  group_val <- lapply(group_var, \(grp){
    dt <- data[[grp]]
    if (is.factor(dt)) return(levels(dt))
    if (length(unique(dt)) < 10 | is.character(dt)) return(sort(unique(dt)))
    if (is.numeric(dt))
      cli::cli_abort(c(
        glue::glue('Group variable {grp} is of type numeric and has too many unique values.'),
        i = 'Perhaps cut it into bins first?'))
  })
  names(group_val) <- group_var
  group_val
}

._tidy_subgroup_ <- function(base_model, grp, data, conf.int, ...){
  group_var <- names(grp)
  group_lv <- grp[[1]]
  group_var_name <- if (!is.null(attr(data[[group_var]], 'label'))) attr(data[[group_var]], 'label') else group_var
  group_fit <- lapply(
    group_lv,
    \(lv) {
      subdat <- dplyr::filter(data, .data[[group_var]] == lv)
      subfit <- update(base_model, data=subdat)
      tidy(subfit, conf.int = conf.int, ...) |>
        dplyr::mutate(.subgroup_val = lv)
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(.subgroup_name = group_var_name ,
                  .subgroup_label = paste(group_var_name, .subgroup_val, sep= ' = ')) |>
    dplyr::select(.subgroup_label, .subgroup_name, .subgroup_val, dplyr::everything())
}

#' @rdname tidy_subgroup
#' @export
as_tidy_subgroup <- function(x, ...){
  UseMethod('as_tidy_subgroup')
}

#' @rdname tidy_subgroup
#' @method as_tidy_subgroup data.frame
#' @export
as_tidy_subgroup.data.frame <- function(x){
  nolegit <- setdiff(c('.subgroup_val', '.subgroup_name', '.subgroup_label', 'estimate'),
                     names(x))
  if (length(nolegit)) cli::cli_abort('x cannot be converted to a tidy_subgroup_tbl.')
  class(x) <- c('tidy_subgroup_tbl', class(x))
  x
}
