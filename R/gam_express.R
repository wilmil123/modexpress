#' Express a single GAM smooth
#'
#' @description
#' Express a single smooth for an `mgcv::gam` object in a similar way to
#' `mgcv::plot.gam` or `gratia::draw.gam`, but allow for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `mgcv::gam`.
#' @param comp_match Which component to match? Works by partial match. For
#' instance, supplying `comp_match = "chla"` will the component with "chla"
#' in the name. For `express_one`, only a single match is allowed.
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default TRUE.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
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
#' @param s_pal "Smooth" palette. For multidimensional smooths, the palette of
#' the heatmap. Must be a `colorspace::scale_fill_continuous_diverging` palette.
#' @param s_pal_rev Reverse the smooth palette? Binary TRUE/FALSE.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param show_rug Should a "rug" be drawn on the bottom edge of the plot?
#' Default in `mgcv::plot.gam` and `gratia::draw.gam` is TRUE, here, it is FALSE.
#' @param ... Unused.
#'
#' @returns A ggplot object of the selected component with partial residuals
#' mapped to the supplied factor and/or covariate.
#'
#' @method express_one gam
#' @export
express_one.gam <- function(model,
                            comp_match,
                            do_regex = TRUE,
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
                            s_pal = "Blue-Red 3",
                            # 3d smooth palette
                            s_pal_rev = FALSE,
                            # whether the 3d smooth palette should be reversed
                            f_pal = "Dynamic",
                            # factor colour palette
                            cov_pal = "Sunset Dark",
                            # covariate colour palette
                            cov_pal_rev = FALSE,
                            # whether the covariate palette should be reversed
                            show_rug = FALSE,
                            ...) {
  orig_data <- unpack_model_data(orig_data, model)

  smooth_name <- grab_smooth_from_regex(model, comp_match, do_regex)

  if (length(smooth_name) > 1) {
    stop(
      paste(
        "Multiple matches to supplied component:",
        paste(smooth_name, collapse = ", "),
        "\nEither refine the search parameter, or, if you want to plot multiple smooths, try `express_many`."
      )
    )
  } else if (length(smooth_name) == 0) {
    stop("No matches to supplied component!")
  }

  out_plot <- plot_gam_component(
    model = model,
    orig_data = orig_data,
    smooth_name = smooth_name,
    by_factor = by_factor,
    by_covar = by_covar,
    b_col = b_col,
    s_col = s_col,
    r_col = r_col,
    r_opac = r_opac,
    s_pal = s_pal,
    s_pal_rev = s_pal_rev,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev,
    show_rug = show_rug
  )

  return(out_plot)
}

#' Express many GAM smooths
#'
#' @description
#' Express multiple smooths from an `mgcv::gam` object in a similar way to
#' `mgcv::plot.gam` or `gratia::draw.gam`, but allow for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `mgcv::gam`.
#' @param comp_match Which components to match? If left NULL, all components of
#' the model will be selects. If supplied, works by successive forward partial
#' matching, splitting at commas. For instance, supplying `comp_match = "chla"`
#' will match any component with "chla" in the name. Supplying
#' `comp_match = "s,toc"` will match a component that contains "s" followed by
#' "toc" anywhere afterwards in the component name, e.g., "s(toc)".
#' Supplying `comp_match = "ti,chla,toc"` will then match "ti(chla, toc)".
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default TRUE.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
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
#' @param s_pal "Smooth" palette. For multidimensional smooths, the palette of
#' the heatmap. Must be a `colorspace::scale_fill_continuous_diverging` palette.
#' @param s_pal_rev Reverse the smooth palette? Binary TRUE/FALSE.
#' @param f_pal "Factor" palette. If mapping an aesthetic to a factor, which
#' palette to use? Must be a
#' `colorspace::scale_color_discrete_qualitative` palette.
#' @param cov_pal "Covariate" palette. If mapping an aesthetic to a covariate,
#' which palette to use? Must be a
#' `colorspace::scale_color_continuous_sequential` palette.
#' @param cov_pal_rev Reverse the covariate palette? Binary TRUE/FALSE.
#' @param show_rug Should a "rug" be drawn on the bottom edge of the plot?
#' Default in `mgcv::plot.gam` and `gratia::draw.gam` is TRUE, here, it is FALSE.
#' @param ... Unused.
#'
#' @returns A `cowplot::plot_grid` object of the selected components with
#' partial residuals mapped to the supplied factor and/or covariate, or if `grid = FALSE`,
#' a list of ggplot objects.
#'
#' @method express_many gam
#' @export
express_many.gam <- function(model,
                             comp_match = NULL,
                             # if left null, will plot all smooths
                             do_regex = TRUE,
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
                             s_pal = "Blue-Red 3",
                             # 3d smooth palette
                             s_pal_rev = FALSE,
                             # whether the 3d smooth palette should be reversed
                             f_pal = "Dynamic",
                             # factor colour palette
                             cov_pal = "Sunset Dark",
                             # covariate colour palette
                             cov_pal_rev = FALSE,
                             # whether the covariate palette should be reversed
                             show_rug = FALSE,
                             ...) {
  orig_data <- unpack_model_data(orig_data, model)

  if (is.null(comp_match)) {
    smooth_names <- unique(gratia::smooth_estimates(model)$`.smooth`)
  } else {
    smooth_names <- grab_smooth_from_regex(model, comp_match, do_regex)

    if (length(smooth_names) == 1) {
      message(
        paste(
          "Only one match found:",
          smooth_names,
          "\nIt may be preferable to use `express_one` with a single smooth because it returns a `ggplot2` object rather than a `cowplot::plot_grid` object, which is less customizable."
        )
      )
    } else if (length(smooth_names) == 0) {
      stop("No matches to supplied component!")
    }
  }

  out_plot_list <- lapply(smooth_names, function(smooth_name) {
    out_plot <- plot_gam_component(
      model = model,
      orig_data = orig_data,
      smooth_name = smooth_name,
      by_factor = by_factor,
      by_covar = by_covar,
      b_col = b_col,
      s_col = s_col,
      r_col = r_col,
      r_opac = r_opac,
      s_pal = s_pal,
      s_pal_rev = s_pal_rev,
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
    names(out_plot_grid) <- smooth_names
  }

  return(out_plot_grid)
}

#' Express a GAM evaluation
#'
#' @description
#' Express the evaluations of an `mgcv::gam` object in a similar way to
#' `mgcv::gam.check` or `gratia::appraise.gam`, but allow for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Included in the output is:
#' 1. A Q-Q plot of model residuals. ([express_qqresid.gam])
#' 2. A linear predictor vs. residuals plot. ([express_linpred.gam])
#' 3. A histogram of residuals. ([express_hist.gam])
#' 4. A fitted vs. observed plot. ([express_fit.gam])
#'
#' @param model A supplied model of type `mgcv::gam`.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
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
#' passed to `mgcv::residuals.gam`.
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
#' @method express_eval gam
#' @export
express_eval.gam <- function(model,
                             orig_data = NULL,
                             by_factor = NULL,
                             by_covar = NULL,
                             grid = TRUE,
                             # whether the plots should be combined
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
  orig_data <- unpack_model_data(orig_data, model)

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

#' Express a GAM fit
#'
#' @description
#' Express the model fit (fitted vs. observed) of an `mgcv::gam` object, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `mgcv::gam`.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
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
#' @returns A ggplot object of the model fit with
#' mapping to the supplied factor and/or covariate.
#'
#' @method express_fit gam
#' @export
express_fit.gam <- function(model,
                            orig_data = NULL,
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

  orig_data <- unpack_model_data(orig_data, model)

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
    gratia::add_fitted(model)

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
        as.character(model$pred.formula), collapse = " "
      ))
    )
  return(out_plot)
}

#' Express a quantile-quantile plot of GAM residuals
#'
#' @description
#' Express a quantile-quantile plot of an `mgcv::gam` object, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `mgcv::gam`.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `mgcv::residuals.gam`.
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
#' @method express_qqresid gam
#' @export
express_qqresid.gam <- function(model,
                                orig_data = NULL,
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
  # local variable definitions
  `.residual` <- qq <- NULL

  orig_data <- unpack_model_data(orig_data, model)

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
    gratia::add_residuals(model, type = res_type)
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
      title = paste("QQ plot of residuals for", outcome_term),
      subtitle = paste("Underlying distribution:", as.character(model$family[1]))
    )
  return(out_plot)
}

#' Express a linear predictor vs. residuals plot for a GAM
#'
#' @description
#' Express a linear predictor vs. residuals plot of an `mgcv::gam` object, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @param model A supplied model of type `mgcv::gam`.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param by_covar A covariate to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. On its own,
#' by_covar will map by a continuous colour scale. If by_factor is also present,
#' by_factor will map by colour and by_covar will map by size.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `mgcv::residuals.gam`.
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
#' @method express_linpred gam
#' @export
express_linpred.gam <- function(model,
                                orig_data = NULL,
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
  orig_data <- unpack_model_data(orig_data, model)

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
    gratia::add_residuals(model, type = res_type)
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

# does not support expressing by covariate
#' Express a histogram of GAM residuals
#'
#' @description
#' Express histogram of residuals for an `mgcv::gam` object, allowing for aestheticizing by factors or
#' covariates potentially not included in the model definition itself.
#'
#' @details
#' Does not currently support faceting by a continuous variable. The function
#' will tell you if by_covar is supplied, that it cannot do this, but it will
#' still return a histogram (or histograms, if by_factor is also supplied).
#'
#' @param model A supplied model of type `mgcv::gam`.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
#' @param by_factor A factor to map aesthetics to. Must be present in the
#' original data. If left NULL, no aesthetic mapping is performed. by_factor
#' will map by colour.
#' @param res_type Residual type. Will work with any of the types that can be
#' passed to `mgcv::residuals.gam`.
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
#' @method express_hist gam
#' @export
express_hist.gam <- function(model,
                             orig_data = NULL,
                             by_factor = NULL,
                             res_type = "deviance",
                             # type of residuals
                             s_col = "black",
                             # smooth line colour
                             r_col = "steelblue3",
                             # residual colour
                             f_pal = "Dynamic",
                             ...) {
  # local variable definitions
  `.residual` <- NULL

  orig_data <- unpack_model_data(orig_data, model)

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
    gratia::add_residuals(model, type = res_type)

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
      title = paste("Residual histogram for", outcome_term),
      subtitle = paste("Underlying distribution:", as.character(model$family[1]))
    )

  return(out_plot)
}

#' Express GAM residuals to gauge systematic effects
#'
#' @description
#' Express the residuals of an `mgcv::gam` object by a factor or covariate, and
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
#' @param model A supplied model of type `mgcv::gam`.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
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
#' passed to `mgcv::residuals.gam`.
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
#' @method express_gauge gam
#' @export
express_gauge.gam <- function(model,
                              orig_data = NULL,
                              by_factor = NULL,
                              by_covar = NULL,
                              covar_fit = "linear",
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
                              ...) {
  # whether the covariate palette should be reversed
  if (is.null(by_factor) &
      is.null(by_covar))
    stop("Both `by_factor` and `by_covar` are NULL. Please supply one or both.")

  orig_data <- unpack_model_data(orig_data, model)

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
    gratia::add_residuals(model, type = res_type)

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

#' Express GAM partial residuals to gauge systematic effects
#'
#' @description
#' Express the partial residuals of an `mgcv::gam` object by a factor or covariate, and
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
#' @param model A supplied model of type `mgcv::gam`.
#' @param comp_match Which components to match? If left NULL, all components of
#' the model will be selects. If supplied, works by successive forward partial
#' matching, splitting at commas. For instance, supplying `comp_match = "chla"`
#' will match any component with "chla" in the name. Supplying
#' `comp_match = "s,toc"` will match a component that contains "s" followed by
#' "toc" anywhere afterwards in the component name, e.g., "s(toc)".
#' Supplying `comp_match = "ti,chla,toc"` will then match "ti(chla, toc)".
#' @param do_regex Should `comp_match` work by selective forward partial matching?
#' Default TRUE.
#' @param orig_data Original data on which the model was built. This can either
#' be supplied with the original data frame directly, or, with `mgcv::gam`
#' objects, the original data can be contained within the model itself by
#' supplying the argument `control = list(keepData = TRUE)`, in which case,
#' orig_data can be left NULL and the data will be taken from the model object.
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
#' @method express_gaugepart gam
#' @export
express_gaugepart.gam <- function(model,
                                  comp_match = NULL,
                                  do_regex = TRUE,
                                  orig_data = NULL,
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
  orig_data <- unpack_model_data(orig_data, model)

  if (length(unique(gratia::smooth_estimates(model)$`.smooth`)) == 1)
    message (
      "The supplied model appears to only have one component. Consider `express_gauge` for single-term models."
    )
  if (is.null(comp_match)) {
    smooth_names <- unique(gratia::smooth_estimates(model)$`.smooth`)
  } else {
    smooth_names <- grab_smooth_from_regex(model, comp_match, do_regex)
    if (length(smooth_names) == 0) {
      stop("No matches to supplied component!")
    }
  }

  if (is.null(by_factor) &
      is.null(by_covar))
    stop("Both `by_factor` and `by_covar` are NULL. Please supply one or both.")

  outcome_term <- model$terms[[2]]
  out_objs <- lapply(smooth_names, function(smooth_name) {
    orig_data <- orig_data |>
      tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
      gratia::add_partial_residuals(model)

    out_obj <- gauge_residuals(
      resid_col = smooth_name,
      outcome_term = outcome_term,
      component = smooth_name,
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

# component must be a string
plot_gam_component <- function(model,
                               orig_data,
                               smooth_name,
                               by_factor,
                               by_covar,
                               b_col,
                               s_col,
                               r_col,
                               r_opac,
                               s_pal,
                               s_pal_rev,
                               f_pal,
                               cov_pal,
                               cov_pal_rev,
                               show_rug) {
  # local variable definitions
  `.smooth` <- NULL

  smooth_funcs <- gratia::smooth_estimates(model) |>
    gratia::add_confint() |>
    subset(.smooth == smooth_name)

  component <- regmatches(smooth_name,
                          regexpr("(?<=\\().*?(?=\\))", smooth_name, perl = TRUE))

  # if the passed component to be plotted contains a comma,
  # i.e., is bivariate
  # captures all of s(x1,x2), ti(x1,x2), te(x1,x2), t2(x1,x2), etc...
  if (grepl(",", component) == TRUE) {
    out_plot <- plot_bivar_smooth(
      orig_data = orig_data,
      model = model,
      component = component,
      smooth_name = smooth_name,
      smooth_funcs = smooth_funcs,
      by_factor = by_factor,
      by_covar = by_covar,
      r_col = r_col,
      r_opac = r_opac,
      s_pal = s_pal,
      s_pal_rev = s_pal_rev,
      f_pal = f_pal,
      cov_pal = cov_pal,
      cov_pal_rev = cov_pal_rev
    )
  } else if (unique(smooth_funcs$`.type`) == "Random effect") {
    # if it is a random effect
    out_plot <- plot_re_smooth(
      orig_data = orig_data,
      model = model,
      component = component,
      smooth_name = smooth_name,
      smooth_funcs = smooth_funcs,
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
    # i.e., is univariate
    out_plot <- plot_univar_smooth(
      orig_data = orig_data,
      model = model,
      component = component,
      smooth_name = smooth_name,
      smooth_funcs = smooth_funcs,
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

plot_re_smooth <- function(orig_data,
                               model,
                               component,
                               smooth_funcs,
                               smooth_name,
                               by_factor,
                               by_covar,
                               s_col,
                               r_col,
                               r_opac,
                               f_pal,
                               cov_pal,
                               cov_pal_rev) {
  # local variable definitions
  `get(component)` <- qq <- `.estimate` <- `.se` <- NULL

  outcome_term <- model$terms[[2]]
  orig_data <- summarize_data_byfactor(orig_data, component)
  smooth_funcs_join <- dplyr::right_join(smooth_funcs[, c(component,
                                                          ".smooth",
                                                          ".type",
                                                          ".by",
                                                          ".estimate",
                                                          ".se",
                                                          ".lower_ci",
                                                          ".upper_ci")], orig_data, by = component)
  smooth_funcs_join <- make_qq(smooth_funcs_join, ".estimate")

  gen_qqline <- make_qqline(smooth_funcs_join, ".estimate")
  qqline_intercept <- gen_qqline[[1]]
  qqline_slope <- gen_qqline[[2]]

  out_plot <- ggplot2::ggplot(smooth_funcs_join)
  out_plot <- build_re_qqplot(out_plot = out_plot,
                              x_dim = "qq",
                              y_dim = ".estimate",
                              error_col = ".se",
                              orig_data = smooth_funcs_join,
                              by_factor = by_factor,
                              by_covar = by_covar,
                              r_col = r_col,
                              r_opac = r_opac,
                              f_pal = f_pal,
                              cov_pal = cov_pal,
                              cov_pal_rev = cov_pal_rev)
  out_plot <- out_plot + ggplot2::geom_abline(slope = qqline_slope,
                                              intercept = qqline_intercept,
                                              colour = s_col) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      y = "Partial effect",
      x = "Gaussian quantiles",
      title = paste(smooth_name, "on", outcome_term),
      caption = paste("Basis:", unique(smooth_funcs$`.type`))
    )
  return(out_plot)
}

plot_univar_smooth <- function(orig_data,
                           model,
                           component,
                           smooth_funcs,
                           smooth_name,
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
  # local variable definitions
  `.lower_ci` <- `.upper_ci` <- `.estimate` <- NULL

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
    gratia::add_partial_residuals(model)

  if (!all(is.na(smooth_funcs$`.by`))) {
    split_by <- unique(smooth_funcs$`.by`)
    cur_split <- unlist(unique(smooth_funcs[, split_by]), use.names = FALSE)
    orig_data <- orig_data |>
      subset(get(split_by) == cur_split)
  }

  out_plot <- ggplot2::ggplot(smooth_funcs) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .lower_ci,
        ymax = .upper_ci,
        x = get(component)
      ),
      alpha = 0.2,
      fill = b_col
    )

  if (show_rug == TRUE) {
    out_plot <- out_plot + ggplot2::geom_rug(
      ggplot2::aes(x = get(component)),
      data = orig_data,
      sides = "b",
      length = grid::unit(0.02, "npc")
    )
  }

  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = component,
    y_dim = smooth_name,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  out_plot <- out_plot + ggplot2::geom_line(ggplot2::aes(x = get(component), y = .estimate), lwd = 1.2) +
    cowplot::theme_minimal_grid() +
    ggplot2::labs(
      y = "Partial effect",
      x = component,
      title = paste(smooth_name, "on", outcome_term),
      caption = paste("Basis:", unique(smooth_funcs$`.type`))
    )
}

plot_bivar_smooth <- function(orig_data,
                           model,
                           component,
                           smooth_funcs,
                           smooth_name,
                           by_factor,
                           by_covar,
                           r_col,
                           r_opac,
                           s_pal,
                           s_pal_rev,
                           f_pal,
                           cov_pal,
                           cov_pal_rev) {
  # local variable definitions
  `.estimate` <- NULL

  outcome_term <- model$terms[[2]]
  orig_data <- orig_data |>
    tidyr::drop_na(tidyselect::any_of(outcome_term)) |>
    gratia::add_partial_residuals(model)

  if (!all(is.na(smooth_funcs$`.by`))) {
    split_by <- unique(smooth_funcs$`.by`)
    cur_split <- unlist(unique(smooth_funcs[, split_by]), use.names = FALSE)
    orig_data <- orig_data |>
      subset(get(split_by) == cur_split)
  }

  x_dim <- strsplit(component, ",")[[1]][1]
  y_dim <- strsplit(component, ",")[[1]][2]

  out_plot <- ggplot2::ggplot(smooth_funcs) +
    ggplot2::geom_raster(ggplot2::aes(
      x = get(x_dim),
      y = get(y_dim),
      fill = .estimate
    )) +
    colorspace::scale_fill_continuous_diverging(palette = s_pal, rev = s_pal_rev)

  out_plot <- add_xy_points(
    out_plot = out_plot,
    x_dim = x_dim,
    y_dim = y_dim,
    orig_data = orig_data,
    by_factor = by_factor,
    by_covar = by_covar,
    r_col = r_col,
    r_opac = r_opac,
    f_pal = f_pal,
    cov_pal = cov_pal,
    cov_pal_rev = cov_pal_rev
  )

  out_plot <- out_plot + cowplot::theme_minimal_grid() +
    ggplot2::labs(
      fill = "Partial effect",
      x = x_dim,
      y = y_dim,
      title = paste(smooth_name, "on", outcome_term),
      caption = paste("Basis:", unique(smooth_funcs$`.type`))
    )

  return(out_plot)
}
