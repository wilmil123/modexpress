collapse_express_bools <- function(orig_data, by_factor, by_covar) {
  if (!is.null(by_factor) & !is.null(by_covar)) {
    by_fac_cov <- "both"
  } else if (!is.null(by_factor) & is.null(by_covar)) {
    by_fac_cov <- "fac"
  } else if (is.null(by_factor) & !is.null(by_covar)) {
    by_fac_cov <- "cov"
  } else if (is.null(by_factor) & is.null(by_covar)) {
    by_fac_cov <- "none"
  } else {
    stop("Something went wrong :(")
  }

  # assert they are of the right type and they exist
  if (by_fac_cov == "both" | by_fac_cov == "fac") {
    if (!(by_factor %in% colnames(orig_data)))
      stop("Could not find supplied factor in original data.")
    fac_in_data <- unlist(orig_data[, by_factor], use.names = FALSE)
    if (!(is.character(fac_in_data) |
          is.factor(fac_in_data)))
      stop("Supplied factor is not a character or factor type.")
  }
  if (by_fac_cov == "both" | by_fac_cov == "cov") {
    if (!(by_covar %in% colnames(orig_data)))
      stop("Could not find supplied covariate in original data.")
    cov_in_data <- unlist(orig_data[, by_covar], use.names = FALSE)
    if (!is.numeric(cov_in_data))
      stop("Supplied covariate is not numeric.")
  }

  return(by_fac_cov)
}

summarize_data_byfactor <- function(orig_data,
                                    comp_match) {

  # local variable initialization
  `get(comp_match)` <- NULL

  # when we need to group RE smooths by data,
  # how can we summarize the data?
  # this function will group by supplied component factor "comp_match"
  # if a column is numeric, it will return the mean of that column for
  # each factor group
  # if a column is not numeric, it will return either unique factor values
  # or a combination of factor values
  # for instance, if grouping by Lake_ID, each Lake_ID has a single ecozone
  # associated with it, because Lake_ID is informally nested within ecozone
  # so, return the unique ecozone for each Lake_ID
  # however, if grouping by ecozone, there are many values of Lake_ID per
  # ecozone. in that case, concatenate the names of all the Lake_IDs in
  # each ecozone, and return that as a "summarized" string
  orig_data <- orig_data |>
    dplyr::group_by(get(comp_match)) |>
    dplyr::summarize(dplyr::across(tidyselect::everything(), function(col) {
      if (is.numeric(col)) {
        return(mean(col))
      } else {
        return(paste(unique(col), collapse = ", "))
      }
    })) |>
    dplyr::select(-`get(comp_match)`)
  return(orig_data)
}

unpack_model_data <- function(orig_data, gam_model) {
  if (is.null(orig_data)) {
    # if original data is not supplied
    orig_data <- gam_model$data # try to grab it from the model object
  }

  if (is.null(orig_data)) {
    # if it is still null
    stop(
      "Could not access model data!\nMake sure to either supply it to the function or to set keepData = TRUE in `gam.control`."
    )
  }
  return(orig_data)
}

pluck_smooth_from_regex <- function(comp_match, gam_model) {
  # regex partial matching stuff
  component_split <- strsplit(comp_match, ",")[[1]]
  component_regex <- purrr::map_chr(component_split, ~ sprintf("(?=.*%s)", .x))
  component_paste <- paste0("^", paste0(component_regex, collapse = ""))

  smooth_name <- grepv(component_paste,
                       unique(gratia::smooth_estimates(gam_model)$`.smooth`),
                       perl = TRUE)

  return(smooth_name)
}

make_qq <- function(data, fact_to_order) {
  data <- data[order(data[[fact_to_order]]), ]
  data$qq <- stats::qnorm(stats::ppoints(nrow(data)))
  return(data)
}

make_qqline <- function(data, var_against) {
  # borrow from base R code for qqline
  qqline_y <- stats::quantile(unlist(data[, var_against], use.names = FALSE), c(0.25, 0.75))
  qqline_x <- stats::qnorm(c(0.25, 0.75))
  qqline_slope <- diff(qqline_y) / diff(qqline_x)
  qqline_intercept <- qqline_y[1L] - qqline_slope * qqline_x[1L]
  return(list(qqline_intercept, qqline_slope))
}

# incorporates from residuals.gam, residuals.lm
lookup_residual_type <- function(select_type) {
  resid_lookup <- c(
    "deviance" = "Deviance",
    "pearson" = "Pearson",
    "scaled.pearson" = "Scaled Pearson",
    "working" = "Working",
    "response" = "Response",
    "standard" = "Standardized",
    "partial" = "Partial"
  )
  return(as.character(resid_lookup[select_type]))
}

add_xy_points <- function(out_plot,
                          x_dim,
                          y_dim,
                          orig_data,
                          by_factor,
                          by_covar,
                          r_col,
                          r_opac,
                          f_pal,
                          cov_pal,
                          cov_pal_rev) {
  by_fac_cov <- collapse_express_bools(orig_data = orig_data,
                                       by_factor = by_factor,
                                       by_covar = by_covar)
  switch(
    by_fac_cov,
    fac = {
      out_plot <- out_plot + ggplot2::geom_point(
        ggplot2::aes(
          x = get(x_dim),
          y = get(y_dim),
          colour = get(by_factor)
        ),
        data = orig_data,
        cex = 1.5,
        alpha = r_opac
      ) +
        colorspace::scale_colour_discrete_qualitative(palette = f_pal) +
        ggplot2::labs(colour = by_factor)
    },
    cov = {
      out_plot <- out_plot + ggplot2::geom_point(
        ggplot2::aes(
          x = get(x_dim),
          y = get(y_dim),
          colour = get(by_covar),
        ),
        data = orig_data,
        cex = 1.5,
        alpha = r_opac
      ) +
        colorspace::scale_colour_continuous_sequential(palette = cov_pal, rev = cov_pal_rev) +
        ggplot2::labs(colour = by_covar)
    },
    both = {
      out_plot <- out_plot + ggplot2::geom_point(
        ggplot2::aes(
          x = get(x_dim),
          y = get(y_dim),
          colour = get(by_factor),
          size = get(by_covar)
        ),
        data = orig_data,
        alpha = r_opac
      ) +
        colorspace::scale_colour_discrete_qualitative(palette = f_pal) +
        ggplot2::labs(colour = by_factor, size = by_covar)
    },
    none = {
      out_plot <- out_plot + ggplot2::geom_point(
        ggplot2::aes(x = get(x_dim), y = get(y_dim)),
        data = orig_data,
        cex = 1.5,
        colour = r_col,
        alpha = r_opac
      )
    },
    stop("Something went wrong :(")
  )

  return(out_plot)
}

build_histogram <- function(out_plot,
                            orig_data,
                            by_factor,
                            resid_col,
                            s_col,
                            r_col,
                            f_pal) {
  if (!is.null(by_factor)) {
    out_plot <- out_plot + ggplot2::geom_histogram(ggplot2::aes(x = get(resid_col),
                                                                fill = get(by_factor)),
                                                   colour = s_col,
                                                   bins = 30) +
      colorspace::scale_fill_discrete_qualitative(palette = f_pal) +
      ggplot2::labs(fill = by_factor) +
      ggplot2::facet_wrap(~ get(by_factor))
  } else {
    out_plot <- out_plot + ggplot2::geom_histogram(ggplot2::aes(x = get(resid_col)),
                                                   fill = r_col,
                                                   colour = s_col,
                                                   bins = 30)
  }
  return(out_plot)
}

build_re_qqplot <- function(out_plot,
                            x_dim,
                            y_dim,
                            error_col,
                            orig_data,
                            by_factor,
                            by_covar,
                            r_col,
                            r_opac,
                            f_pal,
                            cov_pal,
                            cov_pal_rev) {
  # local variable initialization
  error_low <- error_high <- NULL

  orig_data$error_low <- unlist(orig_data[,y_dim], use.names = FALSE) -
    unlist(orig_data[,error_col], use.names = FALSE)
  orig_data$error_high <- unlist(orig_data[,y_dim], use.names = FALSE) +
    unlist(orig_data[,error_col], use.names = FALSE)

  by_fac_cov <- collapse_express_bools(orig_data = orig_data,
                                       by_factor = by_factor,
                                       by_covar = by_covar)
  switch(
    by_fac_cov,
    fac = {
      out_plot <- out_plot + ggplot2::geom_point(ggplot2::aes(x = get(x_dim),
                                                              y = get(y_dim),
                                                              colour = get(by_factor)),
                                                 data = orig_data,
                                                 cex = 1.5,
                                                 alpha = r_opac) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = get(x_dim),
          y = get(y_dim),
          ymin = error_low,
          ymax = error_high,
          colour = get(by_factor)
        ),
        data = orig_data) +
        colorspace::scale_colour_discrete_qualitative(palette = f_pal) +
        ggplot2::labs(colour = by_factor)
    },
    cov = {
      out_plot <- out_plot + ggplot2::geom_point(ggplot2::aes(x = get(x_dim),
                                                              y = get(y_dim),
                                                              colour = get(by_covar)),
                                                 data = orig_data,
                                                 cex = 1.5,
                                                 alpha = r_opac) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = get(x_dim),
          y = get(y_dim),
          ymin = error_low,
          ymax = error_high,
          colour = get(by_covar)
        ),
        data = orig_data) +
        colorspace::scale_colour_continuous_sequential(palette = cov_pal, rev = cov_pal_rev) +
        ggplot2::labs(colour = by_covar)
    },
    both = {
      out_plot <- out_plot + ggplot2::geom_point(ggplot2::aes(x = get(x_dim),
                                                              y = get(y_dim),
                                                              colour = get(by_factor),
                                                              size = get(by_covar)),
                                                 data = orig_data,
                                                 alpha = r_opac) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = get(x_dim),
          y = get(y_dim),
          ymin = error_low,
          ymax = error_high,
          colour = get(by_factor)
        ),
        data = orig_data) +
        colorspace::scale_colour_discrete_qualitative(palette = f_pal) +
        ggplot2::labs(colour = by_factor, size = by_covar)
    },
    none = {
      out_plot <- out_plot + ggplot2::geom_point(ggplot2::aes(x = get(x_dim),
                                                              y = get(y_dim)),
                                                 data = orig_data,
                                                 cex = 1.5,
                                                 colour = r_col,
                                                 alpha = r_opac) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            x = get(x_dim),
            y = get(y_dim),
            ymin = error_low,
            ymax = error_high
            ),
          data = orig_data,
          colour = r_col,
          alpha = r_opac
        )
    }
  )

  return(out_plot)
}

gauge_residuals <- function(resid_col,
                            outcome_term,
                            component,
                            orig_data,
                            by_factor,
                            by_covar,
                            covar_fit,
                            res_type,
                            s_col,
                            r_col,
                            r_opac,
                            f_pal) {
  by_fac_cov <- collapse_express_bools(orig_data = orig_data,
                                       by_factor = by_factor,
                                       by_covar = by_covar)
  if (!is.null(by_factor)) {
    if (!is.null(component)) {
      ptitle <- paste("Partial residuals of", component, "on", outcome_term, "by", by_factor)
      ytitle <- paste(lookup_residual_type(res_type), "residuals of", component)
    } else {
      ptitle <- paste("Residuals for", outcome_term, "by", by_factor)
      ytitle <- paste(lookup_residual_type(res_type), "residuals")
    }
    fac_plot <- ggplot2::ggplot(orig_data) +
      ggplot2::geom_boxplot(ggplot2::aes(
        x = get(by_factor),
        y = get(resid_col),
        fill = get(by_factor)
      )) +
      colorspace::scale_fill_discrete_qualitative(palette = f_pal) +
      cowplot::theme_minimal_grid() +
      ggplot2::labs(
        x = by_factor,
        y = ytitle,
        fill = by_factor,
        title = ptitle
      )

    aovform <- stats::as.formula(paste(paste0("`",resid_col,"`"), "~", by_factor))
    fac_test <- stats::aov(aovform, data = orig_data) |> summary()
  }
  if (!is.null(by_covar)) {
    if (!is.null(component)) {
      ptitle <- paste("Partial residuals of", component, "on", outcome_term, "by", by_covar)
      ytitle <- paste(lookup_residual_type(res_type), "residuals of", component)
    } else {
      ptitle <- paste("Residuals for", outcome_term, "by", by_covar)
      ytitle <- paste(lookup_residual_type(res_type), "residuals")
    }
    switch(covar_fit,
           linear = {
             transform <- paste(paste0("`",resid_col,"`"), "~", by_covar)
             fitform <- stats::as.formula("y ~ x")
           },
           exp = {
             transform <- paste(paste0("`",resid_col,"`"), "~", paste0("exp(", by_covar, ")"))
             fitform <- stats::as.formula("y ~ exp(x)")
           },
           log = {
             transform <- paste(paste0("`",resid_col,"`"), "~", paste0("log(", by_covar, ")"))
             fitform <- stats::as.formula("y ~ log(x)")
           },
           stop("Undefined covariate transform."))

    cov_plot <- ggplot2::ggplot(orig_data)
    cov_plot <- add_xy_points(
      out_plot = cov_plot,
      x_dim = by_covar,
      y_dim = resid_col,
      orig_data = orig_data,
      by_factor = by_factor,
      by_covar = NULL,
      r_col = r_col,
      r_opac = r_opac,
      f_pal = f_pal,
      cov_pal = "Mint",
      cov_pal_rev = TRUE
    )
    cov_plot <- cov_plot +
      ggplot2::geom_smooth(
        ggplot2::aes(x = get(by_covar), y = get(resid_col)),
        method = "lm",
        formula = fitform,
        colour = s_col,
        se = FALSE
      ) +
      cowplot::theme_minimal_grid() +
      ggplot2::labs(
        x = by_covar,
        y = ytitle,
        title = ptitle
      )

    cov_test <- stats::lm(stats::as.formula(transform), data = orig_data) |> summary()
  }

  if (!is.null(by_factor) & !is.null(by_covar)) {
    multivar_form <- paste(transform, "+", paste0("(", by_covar, "|", by_factor, ")"))
    print(multivar_form)
    multivar_test <- lme4::lmer(stats::as.formula(multivar_form), data = orig_data) |> summary()
  }

  switch(by_fac_cov,
         both = {
           output_names <- c(
             paste0(outcome_term, "_by_", by_factor),
             paste0(outcome_term, "_by_", by_covar)
           )
           if(!is.null(component)) {
             output_names <- paste0(component,"_on_",output_names)
           }
           plotlist <- list(fac_plot, cov_plot)
           names(plotlist) <- output_names
           testlist <- list(fac_test, cov_test, multivar_test)
           if(!is.null(component)) {
             names(testlist) <- c(output_names,
                                  paste0(component, "_on_", outcome_term, "_by_", by_factor, "_and_", by_covar))
           } else {
             names(testlist) <- c(output_names,
                                  paste0(outcome_term, "_by_", by_factor, "_and_", by_covar))
           }

         },
         fac = {
           test_name <- (paste0(outcome_term, "_by_", by_factor))
           if(!is.null(component)) {
             test_name <- paste0(component,"_on_",test_name)
           }
           testlist <- list(fac_test)
           names(testlist) <- test_name
           plotlist <- list(fac_plot)
           names(plotlist) <- test_name
         },
         cov = {
           test_name <- (paste0(outcome_term, "_by_", by_covar))
           if(!is.null(component)) {
             test_name <- paste0(component,"_on_",test_name)
           }
           testlist <- list(cov_test)
           names(testlist) <- test_name
           plotlist <- list(cov_plot)
           names(plotlist) <- test_name
         },
         stop("Something went wrong :("))

  out_list <- list("plots" = plotlist,
                   "summaries" = testlist)

  return(out_list)
}
