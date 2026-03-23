###
# Examples of functions using test data (mostly iris or mtcars)
###

## Simple univariate lm ####
lm_1 <- stats::lm(Sepal.Length ~ Sepal.Width, data = iris)
# express fit by Species
express_fit(lm_1, iris, by_factor = "Species")
# express fit by Petal.Length
express_fit(lm_1, iris, by_covar = "Petal.Length")
# express fit by both
express_fit(lm_1, iris, by_factor = "Species", by_covar = "Petal.Length")

# model diagnostics by Species
express_eval(lm_1, iris, by_factor = "Species")
# model diagnostics by Petal.Length
express_eval(lm_1, iris, by_covar = "Petal.Length")

# gauge potential missing parameters by Species
express_gauge(lm_1, iris, by_factor = "Species")
# gauge potential missing parameters by Petal.Length
express_gauge(lm_1, iris, by_covar = "Petal.Length")
# gauge potential missing parameters by both
express_gauge(lm_1, iris, by_factor = "Species", by_covar = "Petal.Length")

## More complex lm with exp(), I(x**2), and interaction terms ####
lm_2 <- stats::lm(
  Sepal.Length ~ exp(Sepal.Width) + I(Petal.Length ** 2) + Sepal.Width * Petal.Length,
  data = iris
)
# express partial fit by Species for one term
express_one(lm_2, iris, "exp(Sepal.Width)", by_factor = "Species")
# express partial fit by Petal.Width for one term
express_one(lm_2, iris, "Sepal.Width:Petal.Length", by_covar = "Petal.Width")

# express partial fit by Species for many terms
express_many(lm_2,
             iris,
             c("exp(Sepal.Width)", "Sepal.Width:Petal.Length"),
             by_factor = "Species")
# express partial fit by covariate for many (all) terms
express_many(lm_2, iris, by_covar = "Petal.Width")

# gauge potential missing parameters in the whole model by Species
express_gauge(lm_2, iris, by_factor = "Species")
# gauge potential missing parameters by partial residuals for one component by Species
express_gaugepart(lm_2, iris, "exp(Sepal.Width)", by_factor = "Species")
# gauge potential missing parameters by partial residuals for many components by Petal.Length
express_gaugepart(lm_2, iris, by_covar = "Petal.Length")

## Simple univariate GAM ####
# `control = list(keepData = TRUE)`
# allows you to not have to specify the original data every time
gam_1 <- mgcv::gam(
  Sepal.Length ~ s(Sepal.Width),
  method = "REML",
  data = iris,
  control = list(keepData = TRUE)
)

# plot the component by Species
express_one(gam_1, "Sepal", by_factor = "Species")

# evaluate by Petal.Width
# note that it will tell you that splitting histograms by a covariate
# is unsupported
express_eval(gam_1, by_covar = "Petal.Width")

## More complex GAM ####
# note the control parameter as above
gam_2 <- mgcv::gam(
  Sepal.Length ~ s(Sepal.Width) + s(Petal.Length) + ti(Sepal.Width, Petal.Length),
  method = "REML",
  data = iris,
  control = list(keepData = TRUE)
)

# it is possible to express single and multivariate smooths
# express the s(Petal.Length) term by Species
express_one(gam_2, "s,Petal", by_factor = "Species")
# express the ti(Sepal.Width, Petal.Length) term
express_one(gam_2, "ti,Se,Pe", by_factor = "Species")
# express everything by Petal.Width
express_many(gam_2, by_covar = "Petal.Width")

# gauge potential missing parameters by Species
express_gauge(gam_2, by_factor = "Species")
# gauge potential missing parameters by partial residuals for one component
express_gaugepart(gam_2, "s,Se", by_factor = "Species")
# express potential missing parameters by partial residuals for many components
express_gaugepart(gam_2, by_factor = "Species")

## GAM with random effect smooth (hierarchical GAM) ####
# note the control parameter as above
gam_3 <- mgcv::gam(
  Sepal.Length ~ s(Sepal.Width) + s(Species, bs = "re"),
  method = "REML",
  data = iris,
  control = list(keepData = TRUE)
)

# show the random effect
express_one(gam_3, "Spec", by_covar = "Petal.Width")
# model evaluation
express_eval(gam_3, by_covar = "Petal.Width")
# gauge potential missing effects
express_gauge(gam_3, by_covar = "Petal.Width")

# GAM with a non-tensor multivariate smooth
# note that it essentially works the same as the tensor product smooths
gam_4 <- mgcv::gam(
  Sepal.Length ~ s(Sepal.Width, Petal.Length),
  method = "REML",
  data = iris,
  control = list(keepData = TRUE)
)

# effectively the same as a tensor smooth but without common scaling
express_many(gam_4)

# with a factor smooth
gam_5 <- mgcv::gam(
  Sepal.Length ~ s(Sepal.Width, Species, bs = "fs"),
  method = "REML",
  data = iris,
  control = list(keepData = TRUE)
)

# note how the factor smooth will show a separate effect for each
# level of the smooth
express_many(gam_5)
# model diagnostics look mostly the same
express_eval(gam_5)

## Simple GLMs ####
# inverse gaussian log-linked
# just to test
glm_1 <- stats::glm(Sepal.Length ~ Sepal.Width,
                    data = iris,
                    family = inverse.gaussian(link = "log"))

glm_2 <- stats::glm(vs ~ wt + disp,
                    data = mtcars,
                    family = binomial(link = "logit"))

# glm objects contain the full data on which they were built
# this means we do not need to pass this parameter at all
# this means however that we do need to feed something to `data` when making the model

# note that the shapes are off for a lot of the plots for a binomial distribution
# however this is also true for the base plot.glm method
express_eval(glm_2)

express_gauge(glm_1, by_factor = "Species")

## More complex GLMs ####
# inverse gaussian log-linked
# just to test
glm_3 <- stats::glm(Sepal.Length ~ Sepal.Width * Petal.Length,
                    data = iris,
                    family = inverse.gaussian(link = "log"))

# express all components by Species
express_many(glm_3, by_factor = "Species")

# gauge by individual parts for the complex model, by Species and Petal.Width
express_gaugepart(glm_3, by_factor = "Species", by_covar = "Petal.Width")

## Linear mixed models ####
lmm_1 <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1|Species),
                    data = iris)

lmm_2 <- lme4::lmer(Sepal.Length ~ Sepal.Width + (Sepal.Width|Species),
                    data = iris)

# express a linear term
express_one(lmm_1, iris, "Sepal.Width")
# express a random term
# note that it is not "(1|Species)", just "Species"
express_one(lmm_1, iris, "Species")
# express a random term with both an intercept and slope
express_one(lmm_2, iris, "Species")
# express all components
express_many(lmm_2, iris)

# for LMMs, express_eval() is similar to the GAM method
# note that plot.lmerMod returns a single plot of fitted values vs. residuals
# that is equivalent to express_linpred() and is included in the evaluation
express_eval(lmm_2, iris, by_covar = "Petal.Length")

# gauge a full model
express_gauge(lmm_2, iris, by_covar = "Petal.Length")

# gauging with LMMs is tricky because only the linear terms have partial
# residuals. instead, for random effect terms, the effects are compared between
# another covariate or a factor, if the factor has fewer levels than there are
# random effects.
express_gaugepart(lmm_2, iris, "Species", by_covar = "Petal.Length")

## Generalized linear mixed models ####
glmm_1 <- lme4::glmer(Sepal.Length ~ Sepal.Width + (1|Species),
                      data = iris,
                      family = inverse.gaussian(link = "log"))

# one GLMM component
express_one(glmm_1, iris, "Sepal.Width", by_covar = "Petal.Length")
# many GLMM components
express_many(glmm_1, iris, by_covar = "Petal.Length")

# evaluate a GLMM object
express_eval(glmm_1, iris, by_factor = "Species")

# gauge a full model
express_gauge(glmm_1, iris, by_factor = "Species")
# gauge a single model component
express_gaugepart(glmm_1, iris, "Sepal.Width", by_covar = "Petal.Width")
