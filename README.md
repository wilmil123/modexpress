# modexpress

## Description
'modexpress' is an R package that provides methods for plotting and testing model coefficients and diagnostics to assess whether or not there is systematic variation within the model according to categorical or factor variables, whether or not they are used in the model itself.

The package was inspired by [this blog post](https://gavinsimpson.github.io/gratia/articles/custom-plotting.html) in which Gavin Simpson shows how to do this with the ['gratia'](https://github.com/gavinsimpson/gratia) package for generalized additive models (GAMs). 'modexpress' provides simplified methods to do just that, as well as extending this idea to model diagnostics equivalent to the `appraise()` function in 'gratia', using 'ggplot2' graphics in a similar way to 'gratia'. 'modexpress' further extends this idea to [models beyond GAMs](#supported-model-types).

### Supported model types
* Linear models (`stats::lm`)
* Generalized linear models (`stats::glm`)
* Linear mixed models (`lme4::lmer`)
* Generalized linear mixed models (`lme4::glmer`)
* Generalized additive models (`mgcv::gam`)

### Main functions
The main ideas behind the functions in the package are to:
* Express the partial effect/residuals of a model component according to other variables
  * `express_one()` (single component)
  * `express_many()` (multiple components)
* Express model evaluations/checks according to other variables
  * `express_eval()`
* Express model (partial) residuals to gauge potential systematic variation along another variable
  * `express_gauge()` (full model)
  * `express_gaugepart()` (individual model components)
  
## Examples
Non-rigorous testing examples for 'modexpress' functions can be found in the [test-examples.R](examples/test-examples.R) file. A walkthrough of these examples is provided in [test-examples.md](examples/test-examples.md) These use R's built-in data to demonstrate some sample outputs. True testing infrastructure is either not possible or extremely complex, because most functions create 'ggplot2' objects which are simultaneously an unvarying return type and highly data-dependent in their internal structure. 
