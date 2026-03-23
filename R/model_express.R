# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#####
# inspired by https://gavinsimpson.github.io/gratia/articles/custom-plotting.html
# modexpress: express model coefficients and diagnostics along variables that are not present in the model data
# implicitly requires mgcv, gratia

# class function definitions

#' Express a single model component
#'
#' @description
#' Plot a singular model component as a ggplot object that includes aesthetics
#' mapped to multiple variables, whether or not they are included in the actual
#' definition of the model.
#' Currently supported model types include:
#' * `lm`: Plot a selected component of a (multivariate) linear model.
#' Supports interaction terms. ([express_one.lm])
#' * `glm`: Plot a selected component of a (multivariate) generalized linear
#' linear model. Supports interactions terms. ([express_one.glm])
#' * `lmm`: Plot a selected component of an `lme4::lmerMod` model. Supports
#' linear and random effects. ([express_one.lmerMod])
#' * `glmm`: Plot a selected component of an `lme4::glmerMod` model. Supports
#' linear and random effects. ([express_one.glmerMod])
#' * `gam`: Plot a selected smooth of an `mgcv::gam` model. Supports univariate,
#' multivariate, and random effect smooths. ([express_one.gam])
#'
#' @details
#' Generic plotting of model components as ggplot objects with optional
#' additional faceting. Uses cowplot and colorspace along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A ggplot object for a single model component.
#' @export
express_one <- function(model, ...) {
  UseMethod("express_one")
}

#' Express many model components
#'
#' @description
#' Plot multiple model components as a `cowplot::plot_grid` object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Plot selected components of a (multivariate) linear model.
#' Supports interaction terms. ([express_many.lm])
#' * `glm`: Plot selected components of a (multivariate) generalized linear
#' model. Supports interaction terms. ([express_many.glm])
#' * `lmm`: Plot selected components of an `lme4::lmerMod` model. Supports
#' linear and random effects. ([express_many.lmerMod])
#' * `glmm`: Plot selected components of an `lme4::glmerMod` model. Supports
#' linear and random effects. ([express_many.glmerMod])
#' * `gam`: Plot selected smooths of an `mgcv::gam` model. Supports univariate,
#' multivariate, and random effect smooths. ([express_many.gam])
#'
#' @details
#' Generic plotting of model components as ggplot objects with optional
#' additional faceting, within a `cowplot::plot_grid`. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A `cowplot::plot_grid` object for multiple model components.
#' @export
express_many <- function(model, ...) {
  UseMethod("express_many")
}

#' Express a model evaluation
#'
#' @description
#' Plot model evaluations as a `cowplot::plot_grid` object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Plot diagnostics for a (multivariate) linear model. ([express_eval.lm])
#' * `glm`: Plot diagnostics for a (multivariate) generalized linear model.
#' ([express_eval.glm])
#' * `lmm`: Plot diagnostics for an `lme4::lmerMod` model. Supports
#' linear and random effects. ([express_eval.lmerMod])
#' * `glmm`: Plot diagnostics for an `lme4::glmerMod` model. Supports
#' linear and random effects. ([express_eval.glmerMod])
#' * `gam`: Plot diagnostics for an `mgcv::gam` model. ([express_eval.gam])
#'
#' @details
#' Generic plotting of model diagnostics as ggplot objects with optional
#' additional faceting, within a `cowplot::plot_grid`. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A `cowplot::plot_grid` object for model diagnostics.
#' @export
express_eval <- function(model, ...) {
  UseMethod("express_eval")
}

#' Express a model fit
#'
#' @description
#' Plot the fit of a full model as a ggplot object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Fit for a (multivariate) linear model. ([express_fit.lm])
#' * `glm`: Fit for a (multivariate) generalized linear model. ([express_fit.glm])
#' * `lmm`: Fit for an `lme4::lmerMod` model. ([express_fit.lmerMod])
#' * `glmm`: Fit for an `lme4::glmerMod` model. ([express_fit.glmerMod])
#' * `gam`: Fit for an `mgcv::gam` model. ([express_fit.gam])
#'
#' @details
#' Generic plotting of model fits as ggplot objects with optional
#' additional faceting. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A ggplot object for the full model fit.
#' @export
express_fit <- function(model, ...) {
  UseMethod("express_fit")
}

#' Express a quantile-quantile plot of model residuals
#'
#' @description
#' Plot the quantile-quantile plot of full model residuals as a ggplot object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Residual Q-Q plot for a (multivariate) linear model. ([express_qqresid.lm])
#' * `glm`: Residual Q-Q plot for a (multivariate) generalized linear model. ([express_qqresid.glm])
#' * `lmm`: Residual Q-Q plot for an `lme4::lmerMod` model. ([express_qqresid.lmerMod])
#' * `glmm`: Residual Q-Q plot for an `lme4::glmerMod` model. ([express_qqresid.glmerMod])
#' * `gam`: Residual Q-Q plot for an `mgcv::gam` model. ([express_qqresid.gam])
#'
#' @details
#' Generic plotting of model residual Q-Q plots as ggplot objects with optional
#' additional faceting. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A ggplot object for the model residual q-q plot.
#' @export
express_qqresid <- function(model, ...) {
  UseMethod("express_qqresid")
}

#' Express a model linear predictor vs. residuals plot
#'
#' @description
#' Plot the linear predictor vs. residuals for a full model as a ggplot object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Linear predictor (a.k.a. fitted values) vs. residuals for a (multivariate) linear model. ([express_linpred.lm])
#' * `glm`: Linear predictor (a.k.a. fitted values) vs. residuals for a
#' (multivariate) generalized linear model. ([express_linpred.glm])
#' * `lmm`: Linear predictor (a.k.a . fitted values) vs. residuals for
#' an `lme4::lmerMod` model. ([express_linpred.lmerMod])
#' * `glmm`: Linear predictor (a.k.a . fitted values) vs. residuals for
#' an `lme4::glmerMod` model. ([express_linpred.glmerMod])
#' * `gam`: Linear predictor vs. residuals plot for an `mgcv::gam` model. ([express_linpred.gam])
#'
#' @details
#' Generic plotting of model linear predictor vs. residual plots as ggplot objects with optional
#' additional faceting. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A ggplot object for the model linear predictor vs. residuals plot.
#' @export
express_linpred <- function(model, ...) {
  UseMethod("express_linpred")
}

#' Express a model residual histogram
#'
#' @description
#' Plot a histogram of full model residuals as a ggplot object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Histogram of residuals for a (multivariate) linear model. ([express_hist.lm])
#' * `glm`: Histogram of residuals for a (multivariate) generalized linear model.
#' ([express_hist.glm])
#' * `lmm`: Histogram of residuals for an `lme4::lmerMod` model. ([express_hist.lmerMod])
#' * `glmm`: Histogram of residuals for an `lme4::glmerMod` model. ([express_hist.glmerMod])
#' * `gam`: Histogram of residuals for an `mgcv::gam` model. ([express_hist.gam])
#'
#' @details
#' Generic plotting of model residual histograms as ggplot objects with optional
#' additional faceting. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A ggplot object for the model residual histogram.
#' @export
express_hist <- function(model, ...) {
  UseMethod("express_hist")
}

#' Express the scale-location plot for a model
#'
#' @description
#' Plot the scale-location of full model (fitted values vs. transformed residuals) as a ggplot object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Scale-location plot for a (multivariate) linear model. ([express_scloc.lm])
#' * `glm`: Scale-location plot for a (multivariate) generalized linear model.
#' ([express_scloc.lm])
#'
#' @details
#' Generic plotting of model scale-locations as ggplot objects with optional
#' additional faceting. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A ggplot object for the model scale-locations.
#' @export
express_scloc <- function(model, ...) {
  UseMethod("express_scloc")
}

#' Express the leverage plot for a model
#'
#' @description
#' Plot the leverage vs. residuals of full model as a ggplot object that includes
#' aesthetics mapped to multiple variables, whether or not they are included in
#' the actual definition of the model.
#' Currently supported model types include:
#' * `lm`: Leverage vs. residuals for a (multivariate) linear model. ([express_lev.lm])
#' * `glm`: Leverage vs. residuals for a (multivariate) generalized linear model.
#' ([express_lev.glm])
#'
#' @details
#' Generic plotting of model residuals vs. leverage as ggplot objects with optional
#' additional faceting. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A ggplot object for the model residuals vs. leverage.
#' @export
express_lev <- function(model, ...) {
  UseMethod("express_lev")
}

#' Express a model's residuals to gauge systematic effects
#'
#' @description
#' Gauge whether there could be potential systematic variation in a model by plotting and
#' analyzing model residuals according to a factor or covariate.
#' Currently supported model types include:
#' * `lm`: Gauge potential missing effects for a (multivariate) linear model. ([express_gauge.lm])
#' * `glm`: Gauge potential missing effects for a (multivariate) generalized
#' linear model. ([express_gauge.glm])
#' * `lmm`: Gauge potential missing effects for an `lme4::lmerMod` model.
#' ([express_gauge.lmerMod])
#' * `glmm`: Gauge potential missing effects for an `lme4::glmerMod` model.
#' ([express_gauge.glmerMod])
#' * `gam`: Gauge potential missing effects for an `mgcv::gam` model. ([express_gauge.gam])
#'
#' @details
#' Generic plotting and analysis of model residuals with
#' additional faceting by a factor or covariate. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A list of plots and model summaries.
#' For more information, see the documentation for each individual method.
#' @export
express_gauge <- function(model, ...) {
  UseMethod("express_gauge")
}

#' Express a model's partial residuals to gauge systematic effects
#'
#' @description
#' Gauge whether there could be potential systematic variation in a model by plotting and
#' analyzing model partial residuals according to a factor or covariate.
#' Currently supported model types include:
#' * `lm`: Gauge potential missing effects for a (multivariate) linear model. ([express_gaugepart.lm])
#' * `glm`: Gauge potential missing effects for a (multivariate) generalized
#' linear model. ([express_gaugepart.glm])
#' * `lmm`: Gauge potential missing effects for an `lme4::lmerMod` model.
#' ([express_gaugepart.lmerMod])
#' * `glmm`: Gauge potential missing effects for an `lme4::glmerMod` model.
#' ([express_gaugepart.glmerMod])
#' * `gam`: Gauge potential missing effects for an `mgcv::gam` model. ([express_gaugepart.gam])
#'
#' @details
#' Generic plotting and analysis of model partial residuals with
#' additional faceting by a factor or covariate. Uses cowplot and colorspace
#' along with ggplot2.
#'
#'
#' @param model A model object.
#' @param ... Arguments passed to specific methods based on model type.
#' @returns A list of plots and model summaries, one for each model component.
#' For more information, see the documentation for each individual method.
#' @export
express_gaugepart <- function(model, ...) {
  UseMethod("express_gaugepart")
}
