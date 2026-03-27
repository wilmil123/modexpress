#' Express an LMM component
#'
#' @description
#' Express a linear or random component of an `lmer` model, allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @details
#' Linear terms will be plotted as their fit-adjusted partial residuals
#' (component + partial residuals). Random terms will be plotted as a Q-Q plot
#' of their effects on the model. If the random term has multiple effects, e.g.,
#' both an intercept and slope, the plot will be faceted for each effect.
#'
#' @param model A supplied model of type `lme4::lmerMod`.
#' @param orig_data Original data on which the model was built.
#' @param comp_match Which component to match? Must fully match only one
#' component present in the model. Component names can be found in, e.g.,
#' `summary.merMod`. Note that effect terms should be referred to by their
#' original name, not their implementation in the model. For instance,
#' if there is a random effect term in a model `(1|Lake_ID)`, refer to this
#' component simply by "Lake_ID", not "(1|Lake_ID)".
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
#' @method express_one lmerMod
#' @export
express_one.lmerMod <- function(model,
                                orig_data,
                                comp_match,
                                do_regex = FALSE,
                                by_factor = NULL,
                                by_covar = NULL,
                                b_col = "black",
                                s_col = "black",
                                r_col = "steelblue3",
                                r_opac = 0.8,
                                f_pal = "Dynamic",
                                cov_pal = "Sunset Dark",
                                cov_pal_rev = FALSE,
                                show_rug = FALSE,
                                ...) {
  if (is.null(orig_data))
    stop ("Could not access original model data! Please supply it to the function.")

  comp_match <- grab_smooth_from_regex(model, comp_match, do_regex)

  if (length(attr(attr(model@frame, "terms"), "term.labels")) == 1)
    message (
      "The supplied model appears to only have one component. Consider `express_fit` for single-term models."
    )

  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]

  # check if we are wanting to plot a random effect or a linear term
  if (is.factor(model@frame[, comp_match]) |
      is.character(model@frame[, comp_match])) {
    out_plot <- plot_mixed_effect(
      model = model,
      orig_data = orig_data,
      comp_match = comp_match,
      outcome_term = outcome_term,
      by_factor = by_factor,
      by_covar = by_covar,
      s_col = s_col,
      r_col = r_col,
      r_opac = r_opac,
      f_pal = f_pal,
      cov_pal = cov_pal,
      cov_pal_rev = cov_pal_rev
    )
  } else {
    out_plot <- plot_lmm_partial_resids(
      model = model,
      orig_data = orig_data,
      comp_match = comp_match,
      outcome_term = outcome_term,
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
  }
  return(out_plot)
}

#' Express many LMM components
#'
#' @description
#' Express multiple linear or random components of an `lmer` model, allowing for
#' aestheticizing by factors or covariates potentially not in the model
#' definition itself.
#'
#' @details
#' Linear terms will be plotted as their fit-adjusted partial residuals
#' (component + partial residuals). Random terms will be plotted as a Q-Q plot
#' of their effects on the model. If the random term has multiple effects, e.g.,
#' both an intercept and slope, the plot will be faceted for each effect.
#'
#' @param model A supplied model of type `lme4::lmerMod`.
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
#' @method express_many lmerMod
#' @export
express_many.lmerMod <- function(model,
                                 orig_data,
                                 comp_match = NULL,
                                 do_regex = FALSE,
                                 by_factor = NULL,
                                 by_covar = NULL,
                                 grid = TRUE,
                                 b_col = "black",
                                 s_col = "black",
                                 r_col = "steelblue3",
                                 r_opac = 0.8,
                                 f_pal = "Dynamic",
                                 cov_pal = "Sunset Dark",
                                 cov_pal_rev = FALSE,
                                 show_rug = FALSE,
                                 ...) {
  if (is.null(comp_match)) {
    comp_names <- attr(attr(model@frame, "terms"), "term.labels")
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
    out_plot <- express_one.lmerMod(
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

#' Express an LMM evaluation
#'
#' @description
#' Express the evaluations of an `lmer` model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Included in the output is:
#' 1. A Q-Q plot of model residuals. ([express_qqresid.lmerMod])
#' 2. A fitted vs. residuals plot. ([express_linpred.lmerMod])
#' 3. A histogram of residuals. ([express_hist.lmerMod])
#' 4. A fitted vs. observed plot. ([express_fit.lmerMod])
#'
#' @param model A supplied model of type `lme4::lmerMod`.
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
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.default`.
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
#' @method express_eval lmerMod
#' @export
express_eval.lmerMod <- function(model,
                                 orig_data = NULL,
                                 by_factor = NULL,
                                 by_covar = NULL,
                                 grid = TRUE,
                                 res_type = "pearson",
                                 s_col = "black",
                                 r_col = "steelblue3",
                                 r_opac = 0.8,
                                 f_pal = "Dynamic",
                                 cov_pal = "Sunset Dark",
                                 cov_pal_rev = FALSE,
                                 ...) {
  if (!is.null(by_covar)) {
    message(
      "Cannot facet histogram by a continuous variable.\nA histogram or histograms will be returned without splitting by a covariate."
    )
  }

  qq <- express_qqresid(
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

  linpred <- express_linpred(
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

  hist_resid <- express_hist(
    model = model,
    orig_data = orig_data,
    by_factor = by_factor,
    res_type = res_type,
    s_col = s_col,
    r_col = r_col,
    f_pal = f_pal
  )

  fits <- express_fit(
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

  if (grid) {
    out_plot_grid <- cowplot::plot_grid(qq, linpred, hist_resid, fits)
  } else {
    out_plot_grid <- list(
      "qqresid" = qq,
      "linpred" = linpred,
      "hist" = hist_resid,
      "fit" = fits
    )
  }

  return(out_plot_grid)
}

#' Express an LMM fit
#'
#' @description
#' Express the fit of an `lmer` model according to its full residuals, allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type `lmerMod`.
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
#' @method express_fit lmerMod
#' @export
express_fit.lmerMod <- function(model,
                                orig_data,
                                by_factor = NULL,
                                by_covar = NULL,
                                s_col = "black",
                                r_col = "steelblue3",
                                r_opac = 0.8,
                                f_pal = "Dynamic",
                                cov_pal = "Sunset Dark",
                                cov_pal_rev = FALSE,
                                ...) {
  model_terms <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[2]]
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]
  orig_data <- remove_orig_data_na(orig_data, model_terms)

  orig_data$`.fitted` <- stats::fitted(model)

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
      caption = paste("Model:", paste(attr(
        attr(model@frame, "terms"), "term.labels"
      ), collapse = " + "))
    )
  return(out_plot)
}

#' Express a quantile-quantile plot of LMM residuals
#'
#' @description
#' Express a quantile-quantile plot of an LMM, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `lmerMod`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.default`.
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
#' @method express_qqresid lmerMod
#' @export
express_qqresid.lmerMod <- function(model,
                                    orig_data,
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
  # local variable definitions
  `.residual` <- qq <- NULL

  model_terms <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[2]]
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]
  orig_data <- remove_orig_data_na(orig_data, model_terms)

  orig_data$`.residual` <- stats::residuals(model, type = res_type)
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

#' Express a linear predictor vs. residuals plot for an LMM
#'
#' @description
#' Express a linear predictor (fitted values) vs. residuals plot of an `lmer`
#' model, allowing for aestheticizing by factors or covariates potentially not
#' included in the model definition itself.
#'
#' @param model A supplied model of type `lmerMod`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.default`.
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
#' @method express_linpred lmerMod
#' @export
express_linpred.lmerMod <- function(model,
                                    orig_data,
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
  model_terms <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[2]]
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]
  orig_data <- remove_orig_data_na(orig_data, model_terms)

  orig_data$`.residual` <- stats::residuals(model, type = res_type)
  orig_data$`.fitted` <- stats::fitted(model)

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

#' Express a histogram of LMM residuals
#'
#' @description
#' Express histogram of residuals for an `lmer` model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Does not currently support faceting by a continuous variable. The function
#' will tell you if by_covar is supplied, that it cannot do this, but it will
#' still return a histogram (or histograms, if by_factor is also supplied).
#'
#' @param model A supplied model of type `lmerMod`.
#' @param orig_data Original data on which the model was built.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `residuals.default`.
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
#' @method express_hist lmerMod
#' @export
express_hist.lmerMod <- function(model,
                                 orig_data,
                                 by_factor = NULL,
                                 res_type = "standard",
                                 s_col = "black",
                                 r_col = "steelblue3",
                                 f_pal = "Dynamic",
                                 ...) {
  # local variable definitions
  `.residual` <- NULL

  model_terms <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[2]]
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]
  orig_data <- remove_orig_data_na(orig_data, model_terms)

  orig_data$`.residual` <- stats::residuals(model, type = res_type)

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

#' Express LMM residuals to gauge systematic effects
#'
#' @description
#' Express the residuals of an `lmer` model by a factor or covariate, and
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
#' @param model A supplied model of type `lme4::lmerMod`.
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
#' passed to `residuals.default`.
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
#' @method express_gauge lmerMod
#' @export
express_gauge.lmerMod <- function(model,
                                  orig_data,
                                  by_factor = NULL,
                                  by_covar = NULL,
                                  covar_fit = "linear",
                                  res_type = "pearson",
                                  s_col = "black",
                                  r_col = "steelblue3",
                                  r_opac = 0.8,
                                  f_pal = "Dynamic",
                                  ...) {
  if (is.null(by_factor) &
      is.null(by_covar))
    stop("Both `by_factor` and `by_covar` are NULL. Please supply one or both.")

  model_terms <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[2]]
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]
  orig_data <- remove_orig_data_na(orig_data, model_terms)

  orig_data$`.residual` <- stats::residuals(model, type = res_type)

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

#' Express LMM partial residuals to gauge systematic effects
#'
#' @description
#' Express the partial residuals of an `lmer` model by a factor or covariate, and
#' test whether there may be systematic effects. One of `by_factor` or `by_covar`
#' (or both) must therefore be supplied.
#'
#' @details
#' If by_factor is supplied, residuals will be tested for systematic variation
#' using a one-way ANOVA between all factor levels. If by_covar is supplied, residuals
#' will be tested for systematic variation by regressing the residuals along
#' the covariate. If both are supplied, both tests will be done, in addition
#' to a mixed effects regression incorporating both the factor and covariate.
#' Random effects will simply compare between random effects for a factor or
#' covariate; linear terms will compare partial residuals.
#'
#' @param model A supplied model of type `lme4::lmerMod`.
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
#' @method express_gaugepart lmerMod
#' @export
express_gaugepart.lmerMod <- function(model,
                                      orig_data,
                                      comp_match = NULL,
                                      do_regex = FALSE,
                                      by_factor = NULL,
                                      by_covar = NULL,
                                      covar_fit = "linear",
                                      s_col = "black",
                                      r_col = "steelblue3",
                                      r_opac = 0.8,
                                      f_pal = "Dynamic",
                                      ...) {
  # local variable initialization
  error_low <- error_high <- grpvar <- NULL

  if (is.null(orig_data))
    stop ("Could not access original model data! Please supply it to the function.")
  if (length(attr(attr(model@frame, "terms"), "term.labels")) == 1)
    message (
      "The supplied model appears to only have one component. Consider `express_gauge` for single-term models."
    )
  if (is.null(comp_match)) {
    comp_names <- attr(attr(model@frame, "terms"), "term.labels")
  } else {
    comp_names <- grab_smooth_from_regex(model, comp_match, do_regex)
  }

  if (is.null(by_factor) &
      is.null(by_covar))
    stop("Both `by_factor` and `by_covar` are NULL. Please supply one or both.")

  model_terms <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[2]]
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]

  out_objs <- lapply(comp_names, function(component) {
    if (!(component %in% attr(attr(model@frame, "terms"), "term.labels")))
      stop (paste("Could not find model component matching", component))

    orig_data <- remove_orig_data_na(orig_data, model_terms)

    # check if we are wanting a random effect or a linear term
    # the random effects in LMMs work slightly different than in GAMs,
    # where they are not smooths, but fixed parameters.
    # partial residuals therefore do not exist (or at least no function will
    # return them for use)
    # you should, however, be able to compare effects between different
    # factors if the factor contains fewer levels than the number of random
    # effect terms
    if (is.factor(model@frame[, component]) |
        is.character(model@frame[, component])) {
      nlevels_in_component <- length(unique(model@frame[, component]))
      nlevels_in_desired <- length(unique(unlist(orig_data[, by_factor], use.names = FALSE)))
      if (nlevels_in_desired > nlevels_in_component)
        message(
          "The number of factor groups in the `by_factor` parameter is larger than the number of random effects.\nThe random effects therefore cannot be grouped by `by_factor`."
        )
      random_effects <- lme4::ranef(model, condVar = TRUE)
      selected_random <- as.data.frame(random_effects) |>
        subset(grpvar == component)
      split_random <- split(selected_random, selected_random$term)

      orig_data <- summarize_data_byfactor(orig_data, component)
      out_obj <- lapply(split_random, function(re) {
        column_match <- component
        names(column_match) <- "grp"

        smooth_funcs_join <- dplyr::right_join(re, orig_data, by = column_match)
        rev_match <- "grp"
        names(rev_match) <- component
        smooth_funcs_join <- dplyr::rename(smooth_funcs_join, tidyr::all_of(rev_match))

        compname <- paste0(unique(smooth_funcs_join$grpvar),
                           ":",
                           unique(smooth_funcs_join$term))

        out_obj <- gauge_residuals(
          resid_col = "condval",
          outcome_term = outcome_term,
          component = compname,
          orig_data = smooth_funcs_join,
          by_factor = by_factor,
          by_covar = by_covar,
          covar_fit = covar_fit,
          res_type = "partial",
          s_col = s_col,
          r_col = r_col,
          r_opac = r_opac,
          f_pal = f_pal
        )

        # fix the naming
        plotnames <- names(out_obj$plots)
        out_obj$plots <- lapply(out_obj$plots, function(re_plot) {
          plot_title <- re_plot$labels$title
          plot_title <- sub("Partial residuals", "Effect", plot_title)
          re_plot <- re_plot + ggplot2::labs(title = plot_title, y = "Effect")
          return(re_plot)
        })
        names(out_obj$plots) <- plotnames

        return(out_obj)
      })
      out_obj <- do.call(Map, c(c, out_obj, use.names = FALSE))
    } else {
      orig_data$partial_resids <- stats::residuals(effects::effect(component, partial.residuals = TRUE, model))
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
    }

  })
  out_objs <- do.call(Map, c(c, out_objs))
  return(out_objs)
}

# superficially resembles the lm method for plotting partial resids
# works a little bit differently to extract information from the LMM model
plot_lmm_partial_resids <- function(model,
                                    orig_data,
                                    comp_match,
                                    outcome_term,
                                    by_factor,
                                    by_covar,
                                    b_col,
                                    s_col,
                                    r_col,
                                    r_opac,
                                    f_pal,
                                    cov_pal,
                                    cov_pal_rev,
                                    show_rug) {
  # local variable initialization
  upper_ci <- lower_ci <- est_line <- NULL

  model_terms <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[2]]
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]
  orig_data <- remove_orig_data_na(orig_data, model_terms)
  orig_data$partial_resids <- stats::residuals(effects::effect(comp_match, partial.residuals = TRUE, model))

  coef_val <- summary(model)$coef[comp_match, 1]
  coef_se <- summary(model)$coef[comp_match, 2]

  # this same portion is very similar express_one.lm
  # however, there are a few side effects that make it complicated for
  # return types
  # also, different ways of accessing internal data
  # consider turning into its own function in the future?
  if (comp_match %in% colnames(orig_data)) {
    # if the component is an unmodified original data field
    comp_toplot <- comp_match
  } else {
    if (grepl(":", comp_match) == TRUE) {
      # if there is a colon in the component name, i.e., is an interaction term
      components_to_mult <- strsplit(comp_match, ":")[[1]]
      orig_data$comp_val <- apply(orig_data[, components_to_mult], 1, prod)
    } else {
      orig_data$comp_val <- model@frame[, comp_match]
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
}

# superficially resembles the GAM method for plotting random effects
# but extends this to work for multiple effects at once, i.e., if there is
# both an intercept and slope random effect
plot_mixed_effect <- function(model,
                              orig_data,
                              comp_match,
                              outcome_term,
                              by_factor,
                              by_covar,
                              s_col,
                              r_col,
                              r_opac,
                              f_pal,
                              cov_pal,
                              cov_pal_rev) {
  # local variable initialization
  grpvar <- qqline_slope <- qqline_intercept <- term <- NULL

  random_effects <- lme4::ranef(model, condVar = TRUE)
  selected_random <- as.data.frame(random_effects) |>
    subset(grpvar == comp_match)
  split_random <- split(selected_random, selected_random$term)

  orig_data <- summarize_data_byfactor(orig_data, comp_match)
  qq_est_re <- lapply(split_random, function(re) {
    column_match <- comp_match
    names(column_match) <- "grp"

    smooth_funcs_join <- dplyr::right_join(re, orig_data, by = column_match)
    rev_match <- "grp"
    names(rev_match) <- comp_match
    smooth_funcs_join <- dplyr::rename(smooth_funcs_join, tidyr::all_of(rev_match))

    re_by_term <- make_qq(smooth_funcs_join, "condval")
    gen_qqline <- make_qqline(re_by_term, "condval")
    re_by_term$qqline_intercept <- gen_qqline[[1]]
    re_by_term$qqline_slope <- gen_qqline[[2]]
    return(re_by_term)
  }) |> dplyr::bind_rows()

  out_plot <- ggplot2::ggplot(qq_est_re)
  out_plot <- build_re_qqplot(
    out_plot = out_plot,
    x_dim = "qq",
    y_dim = "condval",
    error_col = "condsd",
    orig_data = qq_est_re,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )
  # if there is more than one random effect term
  if (length(split_random) > 1) {
    out_plot <- out_plot + ggplot2::facet_wrap( ~ term)
  }

  out_plot <- out_plot + ggplot2::geom_abline(
    ggplot2::aes(slope = qqline_slope, intercept = qqline_intercept),
    colour = s_col,
    data = qq_est_re |>
      dplyr::group_by(term) |>
      dplyr::summarize(
        qqline_slope = unique(qqline_slope),
        qqline_intercept = unique(qqline_intercept)
      )
  ) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      y = "Random effect",
      x = "Gaussian quantiles",
      title = paste(comp_match, "on", outcome_term),
    )
  return(out_plot)
}
