#' Method to create forest plot for \code{tidy_subgroup} tibble
#' @description This function add a method to create forest plot to tidied subgroup models.
#' @author Trinh Dong
#' @param x a tibble create by tidy_subgroup
#' @param terms <character> which term to show forest plot
#' @param label [\code{"Subgroup"}] A character specifying the label of the subgroup column
#' @param .after_stats [\code{c('Est (95% CI)' = '{estimate} ({conf.low}, {conf.high})', 'p-value' =  '{format.pval(p.value, digits=2, eps=1e-3)}')}]
#' stat names in x to be shown in the forest plot after the CI bar
#' @param .before_stats [\code{NULL}] <NULL|character> stat name in x to forest plot that are shown before the CI
#' @param ... additional parameters passed to [forestploter::forest()].
#' @importFrom rlang enquo
#' @return a plot
#' @export
forestplotter <- function(x, ...){
  UseMethod('forestplotter')
}

#' @rdname forestplotter
#' @export
forestplotter.default <- forestploter::forest

#' @rdname forestplotter
#' @method forestplotter tidy_subgroup_tbl
#' @export
forestplotter.tidy_subgroup_tbl <-
  function(x,
           terms = x$term[[2]],
           label ="Subgroup",
           .after_stats = c('Est. (95% CI)' = '{format.ci(estimate, conf.low, conf.high, digits=1)}',
                            'p-value' =  '{format.pval(p.value, digits=2, eps=1e-3)}'),
           .before_stats = NULL,
           .show_p.value = TRUE,
           ...){
    ell <- list(...)
    prepared_dt <- ._prepare_forest_dt_(x, terms, label, .after_stats, .before_stats)
    plt <- with(prepared_dt,
                forestploter::forest(
                  data = prepared_dt |>
                    dplyr::select(-estimate, -conf.low, -conf.high, -std.error,
                           dplyr::any_of(names(.before_stats)),
                           dplyr::any_of(names(.after_stats))) |>
                    dplyr::mutate(
                      dplyr::across(dplyr::everything(),
                                    ~ ifelse(is.na(.x)|.x=='NA','',.x))
                    ),
                  est = estimate,
                  lower = conf.low,
                  upper = conf.high,
                  sizes = std.error,
                  ci_column = if (is.null(ell$ci_column)) 2 else ell$ci_column
                ))
    plt
  }


._prepare_forest_dt_ <- function(x, terms, label, .after_stats, .before_stats){
  dt <- dplyr::filter(x, term %in% terms)
  # dt$.subgroup <- dt$.subgroup_name
  all_stats <-  c(.after_stats, .before_stats)
  for (i in seq_along(all_stats)){
    dt[names(all_stats)[[i]]] <- with(dt,glue::glue(all_stats[[i]]))
  }
  dt <- dplyr::group_by(dt, .subgroup_name) |>
    dplyr::group_modify(
      function(x,y){
        if (all(is.na(x$.subgroup_val))) {
          x$.subgroup_val <-x$.subgroup_label[[1]]
          return(x)
        }
        x1 <- x[1,]
        x1[1,] <- NA
        x1$.subgroup_val <- y[[1]][1]
        x$.subgroup_val <- paste0(' - ', x$.subgroup_val)
        dplyr::bind_rows(x1, x)
      }
    ) |>
    ungroup() |>
    mutate(" " = paste(rep(" ", 30), collapse = " "))

  select(dt, "{label}" := .subgroup_val,
         names(.before_stats),
         ` `,
         estimate,
         std.error, conf.low, conf.high,
         names(.after_stats))
}

format.ci <- function(est, l95, u95, digits=1){
  sprintf(glue::glue('%.{digits}f (%.{digits}f, %.{digits}f)'), est, l95, u95)
}
