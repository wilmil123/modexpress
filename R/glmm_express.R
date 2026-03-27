# NOTE THAT GLMM FUNCTIONS CAN MOSTLY BE PASSED TO LMM EQUIVALENTS WITH A FEW
# TWEAKS

#' Express a GLMM component
#'
#' @description
#' Express a linear or random component of a `glmer` model, allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @details
#' Linear terms will be plotted as their fit-adjusted partial residuals
#' (component + partial residuals). Random terms will be plotted as a Q-Q plot
#' of their effects on the model. If the random term has multiple effects, e.g.,
#' both an intercept and slope, the plot will be faceted for each effect.
#'
#' @param model A supplied model of type `lme4::glmerMod`.
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
#' @method express_one glmerMod
#' @export
express_one.glmerMod <- function(model,
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

  # pass to lmerMod method
  out_plot <- express_one.lmerMod(model = model,
                                  orig_data = orig_data,
                                  comp_match = comp_match,
                                  do_regex = do_regex,
                                  by_factor = by_factor,
                                  by_covar = by_covar,
                                  b_col = b_col,
                                  s_col = s_col,
                                  r_col = r_col,
                                  r_opac = r_opac,
                                  f_pal = f_pal,
                                  cov_pal = cov_pal,
                                  cov_pal_rev = cov_pal_rev,
                                  show_rug = show_rug)
  outcome_term <- attr(attr(attr(model@frame, "terms"), "factors"), "dimnames")[[1]][1]
  out_plot <- out_plot + ggplot2::labs(title = paste("Fit to", outcome_term, "by", comp_match))
  return(out_plot)
}

#' Express many GLMM components
#'
#' @description
#' Express multiple linear or random components of a `glmer` model, allowing for
#' aestheticizing by factors or covariates potentially not in the model
#' definition itself.
#'
#' @details
#' Linear terms will be plotted as their fit-adjusted partial residuals
#' (component + partial residuals). Random terms will be plotted as a Q-Q plot
#' of their effects on the model. If the random term has multiple effects, e.g.,
#' both an intercept and slope, the plot will be faceted for each effect.
#'
#' @param model A supplied model of type `lme4::glmerMod`.
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
#' @method express_many glmerMod
#' @export
express_many.glmerMod <- function(model,
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
    out_plot <- express_one.glmerMod(
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

#' Express a GLMM evaluation
#'
#' @description
#' Express the evaluations of a `glmer` model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Included in the output is:
#' 1. A Q-Q plot of model residuals. ([express_qqresid.glmerMod])
#' 2. A fitted vs. residuals plot. ([express_linpred.glmerMod])
#' 3. A histogram of residuals. ([express_hist.glmerMod])
#' 4. A fitted vs. observed plot. ([express_fit.glmerMod])
#'
#' @param model A supplied model of type `lme4::glmerMod`.
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
#' @method express_eval glmerMod
#' @export
express_eval.glmerMod <- function(model,
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

#' Express a GLMM fit
#'
#' @description
#' Express the fit of an `glmer` model according to its full residuals, allowing for aestheticizing
#' by factors or covariates potentially not in the model definition itself.
#'
#' @param model A supplied model of type `glmerMod`.
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
#' @method express_fit glmerMod
#' @export
express_fit.glmerMod <- function(model,
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
  # pass to lmerMod method
  out_plot <- express_fit.lmerMod(model = model,
                                  orig_data = orig_data,
                                  by_factor = by_factor,
                                  by_covar = by_covar,
                                  s_col = s_col,
                                  r_col = r_col,
                                  r_opac = r_opac,
                                  f_pal = f_pal,
                                  cov_pal = cov_pal,
                                  cov_pal_rev = cov_pal_rev)
  return(out_plot)
}

#' Express a quantile-quantile plot of GLMM residuals
#'
#' @description
#' Express a quantile-quantile plot of a GLMM, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `glmerMod`.
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
#' @method express_qqresid glmerMod
#' @export
express_qqresid.glmerMod <- function(model,
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
  # pass to lmerMod method
  out_plot <- express_qqresid.lmerMod(model = model,
                                      orig_data = orig_data,
                                      by_factor = by_factor,
                                      by_covar = by_covar,
                                      res_type = res_type,
                                      s_col = s_col,
                                      r_col = r_col,
                                      r_opac = r_opac,
                                      f_pal = f_pal,
                                      cov_pal = cov_pal,
                                      cov_pal_rev = cov_pal_rev)
  out_plot <- out_plot + ggplot2::labs(subtitle = paste("Underlying distribution:", as.character(model@resp$family$family)))
  return(out_plot)
}

#' Express a linear predictor vs. residuals plot for a GLMM
#'
#' @description
#' Express a linear predictor (fitted values) vs. residuals plot of a `glmer`
#' model, allowing for aestheticizing by factors or covariates potentially not
#' included in the model definition itself.
#'
#' @param model A supplied model of type `glmerMod`.
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
#' @method express_linpred glmerMod
#' @export
express_linpred.glmerMod <- function(model,
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
  # pass to lmerMod method
  out_plot <- express_linpred.lmerMod(model = model,
                                      orig_data = orig_data,
                                      by_factor = by_factor,
                                      by_covar = by_covar,
                                      res_type = res_type,
                                      s_col = s_col,
                                      r_col = r_col,
                                      r_opac = r_opac,
                                      f_pal = f_pal,
                                      cov_pal = cov_pal,
                                      cov_pal_rev = cov_pal_rev)
  out_plot <- out_plot + ggplot2::labs(subtitle = paste("Underlying distribution:", as.character(model@resp$family$family)))
  return(out_plot)
}

#' Express a histogram of GLMM residuals
#'
#' @description
#' Express histogram of residuals for a `glmer` model, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Does not currently support faceting by a continuous variable. The function
#' will tell you if by_covar is supplied, that it cannot do this, but it will
#' still return a histogram (or histograms, if by_factor is also supplied).
#'
#' @param model A supplied model of type `glmerMod`.
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
#' @method express_hist glmerMod
#' @export
express_hist.glmerMod <- function(model,
                                 orig_data,
                                 by_factor = NULL,
                                 res_type = "standard",
                                 s_col = "black",
                                 r_col = "steelblue3",
                                 f_pal = "Dynamic",
                                 ...) {
  # pass to lmerMod method
  out_plot <- express_hist.lmerMod(model = model,
                                   orig_data = orig_data,
                                   by_factor = by_factor,
                                   res_type = res_type,
                                   s_col = s_col,
                                   r_col = r_col,
                                   f_pal = f_pal)
  out_plot <- out_plot + ggplot2::labs(subtitle = paste("Underlying distribution:", as.character(model@resp$family$family)))
  return(out_plot)
}

#' Express GLMM residuals to gauge systematic effects
#'
#' @description
#' Express the residuals of a `glmer` model by a factor or covariate, and
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
#' @param model A supplied model of type `lme4::glmerMod`.
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
#' @method express_gauge glmerMod
#' @export
express_gauge.glmerMod <- function(model,
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
  # pass to lmerMod method
  out_obj <- express_gauge.lmerMod(model = model,
                                   orig_data = orig_data,
                                   by_factor = by_factor,
                                   by_covar = by_covar,
                                   covar_fit = covar_fit,
                                   res_type = res_type,
                                   s_col = s_col,
                                   r_col = r_col,
                                   r_opac = r_opac,
                                   f_pal = f_pal)
  return(out_obj)
}

#' Express GLMM partial residuals to gauge systematic effects
#'
#' @description
#' Express the partial residuals of an `glmer` model by a factor or covariate, and
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
#' @param model A supplied model of type `lme4::glmerMod`.
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
#' @method express_gaugepart glmerMod
#' @export
express_gaugepart.glmerMod <- function(model,
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
  # pass to lmerMod method
  out_objs <- express_gaugepart.lmerMod(model = model,
                                        orig_data = orig_data,
                                        comp_match = comp_match,
                                        do_regex = do_regex,
                                        by_factor = by_factor,
                                        by_covar = by_covar,
                                        covar_fit = covar_fit,
                                        s_col = s_col,
                                        r_col = r_col,
                                        r_opac = r_opac,
                                        f_pal = f_pal)
  return(out_objs)
}
