#' Method to create forest plot for \code{tidy_subgroup} tibble
#' @description This function add a method to create forest plot to tidied subgroup models.
#' @author Trinh Dong
#' @param x a tibble create by tidy_subgroup
#' @param terms <character> which term to show forest plot
#' @param label [\code{"Subgroup"}] A character specifying the label of the subgroup column
#' @param stats stat names in x to be shown in the forest plot after the CI bar
#' @param plot_pos [\code{2}] <int> column position of plot
#' @param plot_width [\code{1}] <numeric> adjust plot width by a factor > 0
#' @param arrow_lab [\code{c('Harmful', 'Benefit')}] label of the arrow
#' @param ... additional parameters passed to [forestploter::forest()].
#' @param .hide_label [\code{FALSE}] hide subgroup column.
#' @importFrom rlang enquo
#' @return a plot
#' @export
forestploter <- function(x, ...){
  UseMethod('forestploter')
}

#' @rdname forestploter
#' @export
forestploter.default <- forestploter::forest

#' @rdname forestploter
#' @method forestploter tidy_subgroup_tbl
#' @export
forestploter.tidy_subgroup_tbl <-
  function(x,
           terms = x$term[[2]],
           label ="Subgroup",
           stats = c('Est. (95% CI)' = format.ci(estimate, conf.low, conf.high, digits=1),
                     'p-value' =  format.pval(p.value, digits=2, eps=1e-3)),
           plot_pos = 2L,
           plot_width = 1,
           arrow_lab = c('Harmful', 'Benefit'),
           .hide_label = FALSE,
           ...){
    if (plot_width <= 0)
      stop(cli::cli_abort("plot_width must be strictly larger than 0."))
    if (plot_pos <= 1)
      stop(cli::cli_abort("plot_width must be strictly larger than 1."))
    stats <- rlang::enexpr(stats)
    if (as.character(stats[[1]]) == 'c') stats <- stats[-1]
    stats <- stats[names(stats) != '']
    ell <- list(...)
    prepared_dt <- ._prepare_forest_dt_(x, terms, label, stats, plot_pos, plot_width)
    if (.hide_label) prepared_dt[[label]] <- NULL
    plt <- with(prepared_dt,
                forestploter::forest(
                  data = prepared_dt |>
                    dplyr::select(-estimate, -conf.low, -conf.high, -std.error,
                           dplyr::any_of(names(stats))) |>
                    dplyr::mutate(
                      dplyr::across(dplyr::everything(),
                                    ~ ifelse(is.na(.x)|.x=='NA','',.x))
                    ),
                  est = estimate,
                  lower = conf.low,
                  upper = conf.high,
                  sizes = std.error,
                  ci_column = plot_pos - as.numeric(.hide_label),
                  arrow_lab = arrow_lab,
                  ...
                ))
    plt
  }


#' @rdname forestploter
#' @export
#' @return A tibble
as_forest_dt <- function(x, ...){
  UseMethod('as_forest_dt')
}

#' @rdname forestploter
#' @export
as_forest_dt.defautl <- function(x, ...){
  stop(cli::cli_abort('Not implemented.'))
}

#' @rdname forestploter
#' @method as_forest_dt tidy_subgroup_tbl
#' @export
as_forest_dt.tidy_subgroup_tbl <- function(x,
                          terms = x$term[[2]],
                          label = "Subgroup",
                          stats = c('Est. (95% CI)' = format.ci(estimate, conf.low, conf.high, digits=1),
                                    'p-value' =  format.pval(p.value, digits=2, eps=1e-3)),
                          plot_pos = 2L,
                          plot_width = 1){
  stats <- rlang::enexpr(stats)
  if (as.character(stats[[1]]) == 'c') stats <- stats[-1]
  stats <- stats[names(stats) != '']
  ._prepare_forest_dt_(x, terms, label, stats, plot_pos, plot_width)
}

._prepare_forest_dt_ <- function(x, terms, label, stats, plot_pos, plot_width){
  dt <- dplyr::filter(x, term %in% terms)
  # dt$.subgroup <- dt$.subgroup_name
  # browser()
  for (i in seq_along(stats)){
    dt[names(stats)[[i]]] <- with(dt,eval(stats[[i]]))
  }

  dt <- dt |>
    dplyr::group_by(.subgroup_name) |>
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
    ungroup()



  dt <- select(dt, "{label}" := .subgroup_val,
         names(stats)[0:(plot_pos-2)],
         # starts_with('  '),
         estimate,
         std.error, conf.low, conf.high,
         names(stats)[(plot_pos-1):length(stats)],
         everything())
  for (i in seq_along(plot_width)){
    dt <- dplyr::mutate(dt, "{strrep(' ', 2+i)}" := strrep(" ", 30 * plot_width[i]))
    dt <- dplyr::relocate(dt, strrep(' ', 2+i) , .before = plot_pos[i])

  }
  dt

}

format.ci <- function(est, l95, u95, digits=1){
  sprintf(glue::glue('%.{digits}f (%.{digits}f, %.{digits}f)'), est, l95, u95)
}
