# NOTE THAT GLM FUNCTIONS CAN MOSTLY BE PASSED TO LM EQUIVALENTS WITH A FEW
# TWEAKS

#' Express a single GLM component
#'
#' @description
#' Express a single component of a `glm` object according to its fit-adjusted partial
#' residuals (component + partial residuals), allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type `glm`.
#' @param comp_match Which component to match? Must fully match only one
#' component present in the model. Component names can be found in, e.g.,
#' `summary.glm`.
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default FALSE.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
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
#' @method express_one glm
#' @export
express_one.glm <- function(model,
                            comp_match,
                            do_regex = FALSE,
                            orig_data = NULL,
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
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")

  # pass to lm method
  out_plot <- express_one.lm(
    model = model,
    orig_data = orig_data,
    comp_match = comp_match,
    do_regex = do_regex,
    by_factor = by_factor,
    b_col = b_col,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  outcome_term <- model$terms[[2]]
  out_plot <- out_plot + ggplot2::labs(title = paste("Fit to", outcome_term, "by", comp_match))
  return(out_plot)
}

#' Express many GLM components
#'
#' @description
#' Express multiple components of a `glm` object according to their fit-adjusted partial
#' residuals (component + partial residuals), allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type `glm`.
#' @param comp_match Which components to match? If left NULL, all components
#' will be plotted.
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default FALSE.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
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
#' @method express_many glm
#' @export
express_many.glm <- function(model,
                             comp_match = NULL,
                             do_regex = FALSE,
                             orig_data = NULL,
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

    if (length(comp_names) == 1) {
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
    out_plot <- express_one.glm(
      model = model,
      comp_match = comp_name,
      do_regex = FALSE,
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

#' Express a GLM evaluation
#'
#' @description
#' Express the evaluations of a `glm` object in a similar way to
#' `plot.glm`, but allow for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Included in the output is:
#' 1. A linear predictor vs. residuals plot. ([express_linpred.glm])
#' 2. A Q-Q plot of model residuals. ([express_qqresid.glm])
#' 3. A scale-location plot. ([express_scloc.glm])
#' 4. A leverage vs. residuals plot. ([express_lev.glm])
#'
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
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
#' @method express_eval glm
#' @export
express_eval.glm <- function(model,
                             orig_data = NULL,
                             by_factor = NULL,
                             by_covar = NULL,
                             grid = TRUE,
                             s_col = "black",
                             r_col = "steelblue3",
                             r_opac = 0.8,
                             f_pal = "Dynamic",
                             cov_pal = "Sunset Dark",
                             cov_pal_rev = FALSE,
                             ...) {
  qq <- express_qqresid(
    model = model,
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
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = "pearson",
    s_col = s_col,
    r_col = r_col,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  lev <- express_lev(
    model = model,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = "pearson",
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

#' Express a GLM fit
#'
#' @description
#' Express the fit of a `glm` object according to its full residuals, allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
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
#' @method express_fit glm
#' @export
express_fit.glm <- function(model,
                            orig_data = NULL,
                            by_factor = NULL,
                            by_covar = NULL,
                            s_col = "black",
                            r_col = "steelblue3",
                            r_opac = 0.8,
                            f_pal = "Dynamic",
                            cov_pal = "Sunset Dark",
                            cov_pal_rev = FALSE,
                            ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  # pass to lm method
  out_plot <- express_fit.lm(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  return(out_plot)
}

#' Express a quantile-quantile plot of GLM residuals
#'
#' @description
#' Express a quantile-quantile plot of a `glm` object, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.glm`.
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
#' @method express_qqresid glm
#' @export
express_qqresid.glm <- function(model,
                                orig_data = NULL,
                                by_factor = NULL,
                                by_covar = NULL,
                                res_type = "pearson",
                                s_col = "black",
                                r_col = "steelblue3",
                                r_opac = 0.8,
                                f_pal = "Dynamic",
                                cov_pal = "Sunset Dark",
                                cov_pal_rev = FALSE,
                                ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  # pass to lm method
  out_plot <- express_qqresid.lm(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = res_type,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  out_plot <- out_plot + ggplot2::labs(subtitle = paste("Underlying distribution:", as.character(model$family[1])))
  return(out_plot)
}

#' Express a linear predictor vs. residuals plot for a GLM
#'
#' @description
#' Express a linear predictor vs. residuals plot of a `glm` object,
#' allowing for aestheticizing by factors or covariates potentially not included
#' in the model definition itself.
#'
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.glm`, except for "partial".
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
#' @method express_linpred glm
#' @export
express_linpred.glm <- function(model,
                                orig_data = NULL,
                                by_factor = NULL,
                                by_covar = NULL,
                                res_type = "pearson",
                                s_col = "black",
                                r_col = "steelblue3",
                                r_opac = 0.8,
                                f_pal = "Dynamic",
                                cov_pal = "Sunset Dark",
                                cov_pal_rev = FALSE,
                                ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  model_terms <- attr(model$terms, "term.labels")
  outcome_term <- model$terms[[2]]
  orig_data <- remove_orig_data_na(orig_data, model_terms)
  if (res_type == "standard") {
    orig_data$`.residual` <- stats::rstandard(model)
  } else {
    orig_data$`.residual` <- stats::residuals(model, type = res_type)
  }
  orig_data$`.linpred` <- model$linear.predictors

  out_plot <- ggplot2::ggplot(orig_data)
  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = ".linpred",
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
      x = "Linear predictor",
      y = paste(lookup_residual_type(res_type), "residuals"),
      title = paste("Residuals vs. linear predictor for", outcome_term),
      subtitle = paste("Underlying distribution:", as.character(model$family[1]))
    )
  return(out_plot)
}

#' Express a scale-location plot for a GLM
#'
#' @description
#' Express a scale-location (fitted values vs. transformed residuals) plot of a
#' `glm` object, allowing for aestheticizing by factors or covariates
#' potentially not included in the model definition itself.
#'
#' @details
#' Dashed smooth is a LOESS fit through the points.
#'
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.glm`.
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
#' @method express_scloc glm
#' @export
express_scloc.glm <- function(model,
                              orig_data = NULL,
                              by_factor = NULL,
                              by_covar = NULL,
                              res_type = "pearson",
                              s_col = "black",
                              r_col = "steelblue3",
                              r_opac = 0.8,
                              f_pal = "Dynamic",
                              cov_pal = "Sunset Dark",
                              cov_pal_rev = FALSE,
                              ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  # pass to lm method
  out_plot <- express_scloc.lm(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = res_type,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  out_plot <- out_plot + ggplot2::labs(subtitle = paste("Underlying distribution:", as.character(model$family[1])))
  return(out_plot)
}

#' Express a residuals vs. leverage plot for a GLM
#'
#' @description
#' Express a residuals vs. leverage plot for a `glm` object, allowing for
#' aestheticizing by factors or covariates potentially not included in the model
#' definition itself.
#'
#' @details
#' Dashed smooth is a LOESS fit through the points.
#'
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.glm`.
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
#' @method express_lev glm
#' @export
express_lev.glm <- function(model,
                            orig_data = NULL,
                            by_factor = NULL,
                            by_covar = NULL,
                            res_type = "pearson",
                            s_col = "black",
                            r_col = "steelblue3",
                            r_opac = 0.8,
                            f_pal = "Dynamic",
                            cov_pal = "Sunset Dark",
                            cov_pal_rev = FALSE,
                            ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  # pass to lm method
  out_plot <- express_lev.lm(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    res_type = res_type,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  out_plot <- out_plot + ggplot2::labs(subtitle = paste("Underlying distribution:", as.character(model$family[1])))
  return(out_plot)
}

#' Express a histogram of GLM residuals
#'
#' @description
#' Express histogram of residuals for a `glm` object, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Does not currently support faceting by a continuous variable. The function
#' will tell you if by_covar is supplied, that it cannot do this, but it will
#' still return a histogram (or histograms, if by_factor is also supplied).
#'
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.glm`.
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
#' @method express_hist glm
#' @export
express_hist.glm <- function(model,
                             orig_data = NULL,
                             by_factor = NULL,
                             res_type = "deviance",
                             s_col = "black",
                             r_col = "steelblue3",
                             f_pal = "Dynamic",
                             ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  # pass to lm method
  out_plot <- express_hist.lm(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    res_type = res_type,
    s_col = s_col,
    r_col = r_col,
    f_pal = f_pal
  )
  out_plot <- out_plot + ggplot2::labs(subtitle = paste("Underlying distribution:", as.character(model$family[1])))
  return(out_plot)
}

#' Express GLM residuals to gauge systematic effects
#'
#' @description
#' Express the residuals of a `glm` object by a factor or covariate, and
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
#' @param model A supplied model of type `glm`.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
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
#' passed to `residuals.glm`.
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
#' @method express_gauge glm
#' @export
express_gauge.glm <- function(model,
                              orig_data = NULL,
                              by_factor = NULL,
                              by_covar = NULL,
                              covar_fit = "linear",
                              res_type = "deviance",
                              s_col = "black",
                              r_col = "steelblue3",
                              r_opac = 0.8,
                              f_pal = "Dynamic",
                              ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  # pass to lm method
  out_obj <- express_gauge.lm(
    model = model,
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

#' Express GLM partial residuals to gauge systematic effects
#'
#' @description
#' Express the partial residuals of a `glm` object by a factor or covariate, and
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
#' @param model A supplied model of type `glm`.
#' @param comp_match Which components to match? If left NULL, all components
#' will be plotted.
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default FALSE.
#' @param orig_data Original data on which the model was built. If left NULL,
#' it will take from the model object.
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
#' @method express_gaugepart glm
#' @export
express_gaugepart.glm <- function(model,
                                  comp_match = NULL,
                                  do_regex = FALSE,
                                  orig_data = NULL,
                                  by_factor = NULL,
                                  by_covar = NULL,
                                  covar_fit = "linear",
                                  s_col = "black",
                                  r_col = "steelblue3",
                                  r_opac = 0.8,
                                  f_pal = "Dynamic",
                                  ...) {
  if (is.null(orig_data)) {
    orig_data <- model$data
  }
  if (is.null(orig_data))
    stop("Could not access model data. Please make sure to define `data` in the `glm` object.")
  # pass to lm method
  out_objs <- express_gaugepart.lm(
    model = model,
    orig_data = orig_data,
    comp_match = comp_match,
    do_regex = do_regex,
    by_factor = by_factor,
    by_covar = by_covar,
    covar_fit = covar_fit,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal
  )
  return(out_objs)
}
