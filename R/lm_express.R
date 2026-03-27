#' Express a single linear component
#'
#' @description
#' Express a single linear component according to its fit-adjusted partial
#' residuals (component + partial residuals), allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type lm.
#' @param orig_data Original data on which the model was built.
#' @param comp_match Which component to match? Must fully match only one
#' component present in the model. Component names can be found in, e.g.,
#' `summary.lm`.
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default FALSE.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param b_col "Background" colour. Controls the colour of the confidence
#' interval around the parameter estimate.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param show_rug Should a "rug" be drawn on the bottom edge of the plot?
#' @param ... Unused.
#'
#' @returns A ggplot object of the selected component with partial residuals
#' mapped to the supplied factor and/or covariate.
#'
#' @method express_one lm
#' @export
express_one.lm <- function(model,
                           orig_data,
                           comp_match,
                           do_regex = FALSE,
                           by_factor = NULL,
                           by_covar = NULL,
                           b_col = "black",
                           # ribbon colour (background)
                           s_col = "black",
                           # smooth line colour
                           r_col = "steelblue3",
                           # residual colour
                           r_opac = 0.8,
                           # opacity (alpha) of residuals
                           f_pal = "Dynamic",
                           # factor colour palette
                           cov_pal = "Sunset Dark",
                           # covariate colour palette
                           cov_pal_rev = FALSE,
                           # whether the covariate palette should be reversed
                           show_rug = FALSE,
                           ...) {
  # local variable definitions
  upper_ci <- lower_ci <- est_line <- NULL

  if (is.null(orig_data))
    stop ("Could not access original model data! Please supply it to the function.")

  comp_match <- grab_smooth_from_regex(model, comp_match, do_regex)

  if (length(comp_match) == 0) {
    stop("No matches to supplied component!")
  }
  if (length(attr(model$terms, "term.labels")) == 1)
    message (
      "The supplied model appears to only have one component. Consider `express_fit` for single-term models."
    )

  outcome_term <- model$terms[[2]]

  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  orig_data$partial_resids <- stats::residuals(model, type = "partial")[, comp_match]

  coef_val <- summary(model)$coef[comp_match, 1]
  coef_se <- summary(model)$coef[comp_match, 2]

  if (comp_match %in% colnames(orig_data)) {
    # if the component is an unmodified original data field
    comp_toplot <- comp_match
  } else {
    if (grepl(":", comp_match) == TRUE) {
      # if there is a colon in the component name, i.e., is an interaction term
      components_to_mult <- strsplit(comp_match, ":")[[1]]
      orig_data$comp_val <- apply(orig_data[, components_to_mult], 1, prod)
    } else {
      orig_data$comp_val <- model$model[, comp_match]
    }
    # if the term contains I(), it will return as class "AsIs", which cannot be plotted
    # so, coerce to numeric
    orig_data$comp_val <- as.numeric(orig_data$comp_val)
    comp_toplot <- "comp_val"
  }

  orig_data$est_line <- orig_data[, comp_toplot] * coef_val
  orig_data$upper_ci <- orig_data$est_line + (coef_se * 1.96) # assume normal distribution of residuals at 95% C.I.
  orig_data$lower_ci <- orig_data$est_line - (coef_se * 1.96)

  # fix the offset of an unknown intercept
  # is this valid? works though
  orig_data$ccpr <- orig_data$partial_resids + mean(orig_data$est_line, na.rm = TRUE)

  out_plot <- ggplot2::ggplot(orig_data) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = upper_ci,
        ymax = lower_ci,
        x = get(comp_toplot)
      ),
      alpha = 0.2,
      fill = b_col
    )

  if (show_rug == TRUE) {
    out_plot <- out_plot + ggplot2::geom_rug(ggplot2::aes(x = get(comp_toplot)),
                                             sides = "b",
                                             length = grid::unit(0.02, "npc"))
  }

  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = comp_toplot,
    y_dim = "ccpr",
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  out_plot <- out_plot + ggplot2::geom_line(ggplot2::aes(x = get(comp_toplot), y = est_line), colour = b_col) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      x = comp_match,
      y = "Component + Partial residuals",
      title = paste("Linear fit to", outcome_term, "by", comp_match)
    )
  return(out_plot)
}

#' Express many linear components
#'
#' @description
#' Express multiple linear components according to their fit-adjusted partial
#' residuals (component + partial residuals), allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type lm.
#' @param orig_data Original data on which the model was built.
#' @param comp_match Which components to match? If left NULL, all components
#' will be plotted.
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default FALSE.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param grid Whether to return a combined `cowplot::plot_grid` object of all
#' the plots (default = TRUE). If FALSE, will return a list of ggplot objects.
#' @param b_col "Background" colour. Controls the colour of the confidence
#' interval around the parameter estimate.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param show_rug Should a "rug" be drawn on the bottom edge of the plot?
#' @param ... Unused.
#'
#' @returns A `cowplot::plot_grid` object of the selected components with
#' partial residuals mapped to the supplied factor and/or covariate, or if `grid = FALSE`,
#' a list of ggplot objects.
#'
#' @method express_many lm
#' @export
express_many.lm <- function(model,
                            orig_data,
                            comp_match = NULL,
                            do_regex = FALSE,
                            by_factor = NULL,
                            by_covar = NULL,
                            grid = TRUE,
                            b_col = "black",
                            # ribbon colour (background)
                            s_col = "black",
                            # smooth line colour
                            r_col = "steelblue3",
                            # residual colour
                            r_opac = 0.8,
                            # opacity (alpha) of residuals
                            f_pal = "Dynamic",
                            # factor colour palette
                            cov_pal = "Sunset Dark",
                            # covariate colour palette
                            cov_pal_rev = FALSE,
                            # whether the covariate palette should be reversed
                            show_rug = FALSE,
                            ...) {
  if (is.null(comp_match)) {
    comp_names <- attr(model$terms, "term.labels")
  } else {
    comp_names <- grab_smooth_from_regex(model, comp_match, do_regex)

    if (length(comp_names) == 0) {
      stop("No matches to supplied component!")
    } else if (length(comp_names) == 1) {
      message(
        paste(
          "Only one component supplied:",
          comp_names,
          "\nIt may be preferable to use `express_one` with a single component because it returns a `ggplot2` object rather than a `cowplot::plot_grid` object, which is less customizable."
        )
      )
    }
  }

  out_plot_list <- lapply(comp_names, function(comp_name) {
    out_plot <- express_one.lm(
      model = model,
      comp_match = comp_name,
      orig_data = orig_data,
      by_factor = by_factor,
      by_covar = by_covar,
      b_col = b_col,
      s_col = s_col,
      r_col = r_col,
      r_opac = r_opac,
      f_pal = f_pal,
      cov_pal = cov_pal,
      cov_pal_rev = cov_pal_rev,
      show_rug = show_rug
    )
    return(out_plot)
  })

  if (grid) {
    out_plot_grid <- cowplot::plot_grid(plotlist = out_plot_list)
  } else {
    out_plot_grid <- out_plot_list
    names(out_plot_grid) <- comp_names
  }

  return(out_plot_grid)
}

#' Express a linear model evaluation
#'
#' @description
#' Express the evaluations of an `lm`  object in a similar way to
#' `plot.lm`, but allow for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Included in the output is:
#' 1. A linear predictor (fitted) vs. residuals plot. ([express_linpred.lm])
#' 2. A Q-Q plot of model residuals. ([express_qqresid.lm])
#' 3. A scale-location plot. ([express_scloc.lm])
#' 4. A leverage vs. residuals plot. ([express_lev.lm])
#'
#' @param model A supplied model of type `lm`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param grid Whether to return a combined `cowplot::plot_grid` object of all
#' the plots (default = TRUE). If FALSE, will return a list of ggplot objects.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param ... Unused.
#'
#' @returns A `cowplot::plot_grid` object of the model diagnostics with
#' mapping to the supplied factor and/or covariate, or if `grid = FALSE`,
#' a list of ggplot objects.
#'
#' @method express_eval lm
#' @export
express_eval.lm <- function(model,
                            orig_data,
                            by_factor = NULL,
                            by_covar = NULL,
                            grid = TRUE,
                            # type of residuals
                            s_col = "black",
                            # smooth line colour
                            r_col = "steelblue3",
                            # residual colour
                            r_opac = 0.8,
                            # opacity (alpha) of residuals
                            f_pal = "Dynamic",
                            # factor colour palette
                            cov_pal = "Sunset Dark",
                            # covariate colour palette
                            cov_pal_rev = FALSE,
                            ...) {
  qq <- express_qqresid(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = "standard",
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  linpred <- express_linpred(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = "deviance",
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  scloc <- express_scloc(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = "standard",
    s_col = s_col,
    r_col = r_col,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  lev <- express_lev(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = "standard",
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  if (grid) {
    out_plot_grid <- cowplot::plot_grid(linpred, qq, scloc, lev)
  } else {
    out_plot_grid <- list(
      "linpred" = linpred,
      "qqresid" = qq,
      "scloc" = scloc,
      "lev" = lev
    )
  }

  return(out_plot_grid)
}


#' Express a linear model fit
#'
#' @description
#' Express the fit of a linear model according to its full residuals, allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type lm.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' model fit.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param ... Unused.
#'
#' @returns A ggplot object of the full model fit
#' mapped to the supplied factor and/or covariate.
#'
#' @method express_fit lm
#' @export
express_fit.lm <- function(model,
                           orig_data,
                           by_factor = NULL,
                           by_covar = NULL,
                           s_col = "black",
                           # smooth line colour
                           r_col = "steelblue3",
                           # residual colour
                           r_opac = 0.8,
                           # opacity (alpha) of residuals
                           f_pal = "Dynamic",
                           # factor colour palette
                           cov_pal = "Sunset Dark",
                           # covariate colour palette
                           cov_pal_rev = FALSE,
                           ...) {
  # whether the covariate palette should be reversed

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  orig_data$`.fitted` <- model$fitted.values

  out_plot <- ggplot2::ggplot(orig_data)
  # add_xy_points does double duty here to plot true:fitted values
  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = ".fitted",
    y_dim = outcome_term,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  out_plot <- out_plot + ggplot2::geom_abline(slope = 1,
                                              intercept = 0,
                                              colour = s_col) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      x = paste("Fitted", outcome_term),
      y = paste("Observed", outcome_term),
      title = paste("Full model fit for", outcome_term),
      caption = paste("Model:", paste(
        attr(model$terms, "term.labels"), collapse = " + "
      ))
    )
  return(out_plot)
}

#' Express a quantile-quantile plot of linear model residuals
#'
#' @description
#' Express a quantile-quantile plot of a linear model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type lm.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.lm`, or "standard" (default) for standardized residuals.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param ... Unused.
#'
#' @returns A ggplot object of the residual Q-Q plot with
#' mapping to the supplied factor and/or covariate.
#'
#' @method express_qqresid lm
#' @export
express_qqresid.lm <- function(model,
                               orig_data,
                               by_factor = NULL,
                               by_covar = NULL,
                               res_type = "standard",
                               s_col = "black",
                               # smooth line colour
                               r_col = "steelblue3",
                               # residual colour
                               r_opac = 0.8,
                               # opacity (alpha) of residuals
                               f_pal = "Dynamic",
                               # factor colour palette
                               cov_pal = "Sunset Dark",
                               # covariate colour palette
                               cov_pal_rev = FALSE,
                               ...) {
  # local variable definitions
  `.residual` <- qq <- NULL

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  if (res_type == "standard") {
    orig_data$`.residual` <- stats::rstandard(model)
  } else {
    orig_data$`.residual` <- stats::residuals(model, type = res_type)
  }
  orig_data <- make_qq(orig_data, ".residual")

  gen_qqline <- make_qqline(orig_data, ".residual")
  qqline_intercept <- gen_qqline[[1]]
  qqline_slope <- gen_qqline[[2]]

  out_plot <- ggplot2::ggplot(orig_data)
  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = "qq",
    y_dim = ".residual",
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  out_plot <- out_plot + ggplot2::geom_abline(slope = qqline_slope,
                                              intercept = qqline_intercept,
                                              colour = s_col) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      y = paste(lookup_residual_type(res_type), "residuals"),
      x = "Theoretical quantiles",
      title = paste("QQ plot of residuals for", outcome_term)
    )
  return(out_plot)
}

#' Express a linear predictor vs. residuals plot for a linear model
#'
#' @description
#' Express a linear predictor (fitted values) vs. residuals plot of a linear model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `lm`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.lm`, except for "partial".
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param ... Unused.
#'
#' @returns A ggplot object of the linear predictor vs. residuals plot with
#' mapping to the supplied factor and/or covariate.
#'
#' @method express_linpred lm
#' @export
express_linpred.lm <- function(model,
                               orig_data,
                               by_factor = NULL,
                               by_covar = NULL,
                               res_type = "deviance",
                               # type of residuals
                               s_col = "black",
                               # smooth line colour
                               r_col = "steelblue3",
                               # residual colour
                               r_opac = 0.8,
                               # opacity (alpha) of residuals
                               f_pal = "Dynamic",
                               # factor colour palette
                               cov_pal = "Sunset Dark",
                               # covariate colour palette
                               cov_pal_rev = FALSE,
                               ...) {
  # whether the covariate palette should be reversed

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  if (res_type == "standard") {
    orig_data$`.residual` <- stats::rstandard(model)
  } else {
    orig_data$`.residual` <- stats::residuals(model, type = res_type)
  }
  orig_data$`.fitted` <- model$fitted.values

  out_plot <- ggplot2::ggplot(orig_data)
  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = ".fitted",
    y_dim = ".residual",
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  out_plot <- out_plot + ggplot2::geom_abline(slope = 0,
                                              intercept = 0,
                                              colour = s_col) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      x = paste("Fitted", outcome_term),
      y = paste(lookup_residual_type(res_type), "residuals"),
      title = paste("Residuals vs. fitted values for", outcome_term)
    )

  return(out_plot)
}

#' Express a scale-location plot for a linear model
#'
#' @description
#' Express a scale-location (fitted values vs. transformed residuals) plot of a linear model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Dashed smooth is a LOESS fit through the points.
#'
#' @param model A supplied model of type `lm`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.lm`, or "standard" (default) for standardized residuals.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param ... Unused.
#'
#' @returns A ggplot object of the scale-location plot with
#' mapping to the supplied factor and/or covariate.
#'
#' @method express_scloc lm
#' @export
express_scloc.lm <- function(model,
                             orig_data,
                             by_factor = NULL,
                             by_covar = NULL,
                             res_type = "standard",
                             s_col = "black",
                             r_col = "steelblue3",
                             # residual colour
                             r_opac = 0.8,
                             # opacity (alpha) of residuals
                             f_pal = "Dynamic",
                             # factor colour palette
                             cov_pal = "Sunset Dark",
                             # covariate colour palette
                             cov_pal_rev = FALSE,
                             ...) {
  # local variable definitions
  `.fitted` <- `.scloc` <- NULL

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  if (res_type == "standard") {
    orig_data$`.residual` <- stats::rstandard(model)
  } else {
    orig_data$`.residual` <- stats::residuals(model, type = res_type)
  }
  orig_data$`.scloc` <- sqrt(abs(orig_data$`.residual`))
  orig_data$`.fitted` <- model$fitted.values

  out_plot <- ggplot2::ggplot(orig_data)
  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = ".fitted",
    y_dim = ".scloc",
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  out_plot <- out_plot + ggplot2::geom_smooth(
    ggplot2::aes(x = `.fitted`, y = `.scloc`),
    method = "loess",
    formula = y ~ x,
    linetype = "dashed",
    colour = s_col,
    se = FALSE,
  ) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      x = paste("Fitted", outcome_term),
      y = bquote(sqrt(.(
        paste0("|", lookup_residual_type(res_type), " residuals|")
      ))),
      title = paste("Scale-location for", outcome_term)
    )

  return(out_plot)
}

#' Express a residuals vs. leverage plot for a linear model
#'
#' @description
#' Express a residuals vs. leverage plot for a linear model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Dashed smooth is a LOESS fit through the points.
#'
#' @param model A supplied model of type `lm`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.lm`, or "standard" (default) for standardized residuals.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param ... Unused.
#'
#' @returns A ggplot object of the residuals vs. leverage plot with
#' mapping to the supplied factor and/or covariate.
#'
#' @method express_lev lm
#' @export
express_lev.lm <- function(model,
                           orig_data,
                           by_factor = NULL,
                           by_covar = NULL,
                           res_type = "standard",
                           s_col = "black",
                           r_col = "steelblue3",
                           # residual colour
                           r_opac = 0.8,
                           # opacity (alpha) of residuals
                           f_pal = "Dynamic",
                           # factor colour palette
                           cov_pal = "Sunset Dark",
                           # covariate colour palette
                           cov_pal_rev = FALSE,
                           ...) {
  # local variable definitions
  `.residual` <- `.lev` <- NULL

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  if (res_type == "standard") {
    orig_data$`.residual` <- stats::rstandard(model)
  } else {
    orig_data$`.residual` <- stats::residuals(model, type = res_type)
  }
  orig_data$`.lev` <- stats::hatvalues(model)

  out_plot <- ggplot2::ggplot(orig_data)
  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = ".lev",
    y_dim = ".residual",
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  out_plot <- out_plot + ggplot2::geom_smooth(
    ggplot2::aes(x = `.lev`, y = `.residual`),
    method = "loess",
    formula = y ~ x,
    linetype = "dashed",
    colour = s_col,
    se = FALSE
  ) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      x = "Leverage",
      y = paste(lookup_residual_type(res_type), "residuals"),
      title = paste("Residuals vs. leverage for", outcome_term)
    )

  return(out_plot)
}

#' Express a histogram of linear model residuals
#'
#' @description
#' Express histogram of residuals for an `lm` object, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Does not currently support faceting by a continuous variable. The function
#' will tell you if by_covar is supplied, that it cannot do this, but it will
#' still return a histogram (or histograms, if by_factor is also supplied).
#'
#' @param model A supplied model of type `lm`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.lm`, or "standard" (default) for standardized residuals.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' parameter estimate.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param ... Unused.
#'
#' @returns A ggplot object for a histogram of residuals, potentially faceted
#' by the supplied factor.
#'
#' @method express_hist lm
#' @export
express_hist.lm <- function(model,
                            orig_data,
                            by_factor = NULL,
                            res_type = "standard",
                            # type of residuals
                            s_col = "black",
                            # smooth line colour
                            r_col = "steelblue3",
                            # residual colour
                            f_pal = "Dynamic",
                            ...) {
  # local variable definitions
  `.residual` <- NULL

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  if (res_type == "standard") {
    orig_data$`.residual` <- stats::rstandard(model)
  } else {
    orig_data$`.residual` <- stats::residuals(model, type = res_type)
  }

  out_plot <- ggplot2::ggplot(orig_data)
  out_plot <- build_histogram(
    out_plot = out_plot,
    orig_data = orig_data,
    by_factor = by_factor,
    resid_col = ".residual",
    s_col = s_col,
    r_col = r_col,
    f_pal = f_pal
  )

  out_plot <- out_plot + cowplot::theme_minimal_grid() +
    ggplot2::labs(
      x = paste(lookup_residual_type(res_type), "residuals"),
      y = "Frequency",
      title = paste("Residual histogram for", outcome_term)
    )

  return(out_plot)
}

#' Express linear model residuals to gauge systematic effects
#'
#' @description
#' Express the residuals of a linear model by a factor or covariate, and
#' test whether there may be systematic effects. One of `by_factor` or `by_covar`
#' (or both) must therefore be supplied.
#'
#' @details
#' If by_factor is supplied, residuals will be tested for systematic variation
#' using a one-way ANOVA between all factor levels. If by_covar is supplied, residuals
#' will be tested for systematic variation by regressing the residuals along
#' the covariate. If both are supplied, both tests will be done, in addition
#' to a multivariate regression incorporating both the factor and covariate.
#'
#' @param model A supplied model of type `lm`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour. Residuals will be tested for systematic variation
#' using a one-way ANOVA.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size. Residuals will
#' be tested for systematic variation by regressing the residuals along the
#' covariate. See `covar_fit`.
#' @param covar_fit Specify a transform for the covariate for regression.
#' If there is a presumed relatonship between the covariate
#' and the residuals, might it be nonlinear? Default is
#' `"linear"` (i.e., no transform) but can be any of `c("linear", "exp", "log")`.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.lm`, or "standard" (default) for standardized residuals.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' regression estimate of the covariate and residuals.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_fill_discrete_qualitative` palette.
#' @param ... Unused.
#'
#' @returns A named list containing:
#' * `"plots"`: If using by_factor, a boxplot of the residuals by factor group.
#' If using by_covar, a scatterplot of the residuals along the covariate. If
#' using both, both. Plots are named by the dependent variable of the model and
#' either the factor or covariate split for easy differentiation.
#' * `"summaries"` If using by_factor, the summary of the one-way ANOVA for the
#' residuals as they vary by factor group. If using by_covar, the summary of the
#' linear regression on the residuals by the covariate. If using both, both of
#' the previous tests plus a multivariate regression by the interaction of the
#' covariate and the factor on the residuals. See details. Summaries are named by
#' the dependent variable of the model and the factor and/or covariate split
#' for easy differentiation.
#'
#' @method express_gauge lm
#' @export
express_gauge.lm <- function(model,
                             orig_data,
                             by_factor = NULL,
                             by_covar = NULL,
                             covar_fit = "linear",
                             res_type = "standard",
                             # type of residuals
                             s_col = "black",
                             # smooth line colour
                             r_col = "steelblue3",
                             # residual colour
                             r_opac = 0.8,
                             # opacity (alpha) of residuals
                             f_pal = "Dynamic",
                             # factor colour palette
                             ...) {
  # whether the covariate palette should be reversed
  if (is.null(by_factor) &
      is.null(by_covar))
    stop("Both `by_factor` and `by_covar` are NULL. Please supply one or both.")

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term))
  if (res_type == "standard") {
    orig_data$`.residual` <- stats::rstandard(model)
  } else {
    orig_data$`.residual` <- stats::residuals(model, type = res_type)
  }

  out_obj <- gauge_residuals(
    resid_col = ".residual",
    outcome_term = outcome_term,
    component = NULL,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    covar_fit = covar_fit,
    res_type = res_type,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal
  )
  return(out_obj)
}

#' Express linear model partial residuals to gauge systematic effects
#'
#' @description
#' Express the partial residuals of a linear model by a factor or covariate, and
#' test whether there may be systematic effects. One of `by_factor` or `by_covar`
#' (or both) must therefore be supplied.
#'
#' @details
#' If by_factor is supplied, residuals will be tested for systematic variation
#' using a one-way ANOVA between all factor levels. If by_covar is supplied, residuals
#' will be tested for systematic variation by regressing the residuals along
#' the covariate. If both are supplied, both tests will be done, in addition
#' to a mixed effects regression incorporating both the factor and covariate.
#'
#' @param model A supplied model of type `lm`.
#' @param orig_data Original data on which the model was built.
#' @param comp_match Which components to match? If left NULL, all components
#' will be plotted.
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default FALSE.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour. Residuals will be tested for systematic variation
#' using a one-way ANOVA.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size. Residuals will
#' be tested for systematic variation by regressing the residuals along the
#' covariate. See `covar_fit`.
#' @param covar_fit Specify a transform for the covariate for regression.
#' If there is a presumed relatonship between the covariate
#' and the residuals, might it be nonlinear? Default is
#' `"linear"` (i.e., no transform) but can be any of `c("linear", "exp", "log")`.
#' @param s_col "Smooth" colour. Controls the colour of the line drawn for the
#' regression estimate of the covariate and residuals.
#' @param r_col "Residual" colour. Controls the colour of the residuals drawn
#' for the model component.
#' @param r_opac "Residual" opacity. Controls the opacity of the residuals drawn
#' for the model component.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_fill_discrete_qualitative` palette.
#' @param ... Unused.
#'
#' @returns A named list containing:
#' * `"plots"`: At least one for each component passed to the function.
#' If using by_factor, a boxplot of the residuals by factor group.
#' If using by_covar, a scatterplot of the residuals along the covariate. If
#' using both, both. Plots are named by the component, the dependent variable
#' of the model, and either the factor or covariate split for easy differentiation.
#' * `"summaries"` At least one for each component passed to the function.
#' If using by_factor, the summary of the one-way ANOVA for the
#' residuals as they vary by factor group. If using by_covar, the summary of the
#' linear regression on the residuals by the covariate. If using both, both of
#' the previous tests plus a linear mixed model of the covariate on the
#' residuals with a random effect of the factor. See details. Summaries are
#' named by the component, the dependent variable of the model, and the factor
#' and/or covariate split for easy differentiation.
#'
#' @method express_gaugepart lm
#' @export
express_gaugepart.lm <- function(model,
                                 orig_data,
                                 comp_match = NULL,
                                 do_regex = FALSE,
                                 by_factor = NULL,
                                 by_covar = NULL,
                                 covar_fit = "linear",
                                 # type of residuals
                                 s_col = "black",
                                 # smooth line colour
                                 r_col = "steelblue3",
                                 # residual colour
                                 r_opac = 0.8,
                                 # opacity (alpha) of residuals
                                 f_pal = "Dynamic",
                                 # factor colour palette
                                 ...) {
  if (is.null(orig_data))
    stop ("Could not access original model data! Please supply it to the function.")
  if (length(attr(model$terms, "term.labels")) == 1)
    message (
      "The supplied model appears to only have one component. Consider `express_gauge` for single-term models."
    )
  if (is.null(comp_match)) {
    comp_names <- attr(model$terms, "term.labels")
  } else {
    comp_names <- grab_smooth_from_regex(model, comp_match, do_regex)
  }

  if (length(comp_names) == 0) {
    stop("No matches to supplied component!")
  }

  if (is.null(by_factor) &
      is.null(by_covar))
    stop("Both `by_factor` and `by_covar` are NULL. Please supply one or both.")

  outcome_term <- model$terms[[2]]
  out_objs <- lapply(comp_names, function(component) {
    if (!(component %in% attr(model$terms, "term.labels")))
      stop (paste("Could not find model component matching", component))

    orig_data <- orig_data |>
      tidyr::drop_na(tidyselect::any_of(outcome_term))
    orig_data$partial_resids <- stats::residuals(model, type = "partial")[, component]
    out_obj <- gauge_residuals(
      resid_col = "partial_resids",
      outcome_term = outcome_term,
      component = component,
      orig_data = orig_data,
      by_factor = by_factor,
      by_covar = by_covar,
      covar_fit = covar_fit,
      res_type = "partial",
      s_col = s_col,
      r_col = r_col,
      r_opac = r_opac,
      f_pal = f_pal
    )

  })
  out_objs <- do.call(Map, c(c, out_objs))
  return(out_objs)
}
