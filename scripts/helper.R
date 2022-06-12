
# libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(purrr)
library(mvtnorm)
library(tidyr)
library(magrittr)
library(tidymodels)
library(rpart.plot)

# Model specific libraries
library(discrim)
library(C50)
library(rpart)
library(LiblineaR)
library(kernlab)
library(keras)
library(nnet)

# Dataset and bayes optimal boundary plot ---------------------------------

#' @name: dataset generator
#'
#' @param size numeric:
#' @param nVar numeric:
#' @param n_g numeric:
#' @param class_fun function:
#' @param treshold numeric:
#' @return results list: list(dataset_plot = dataset_plot, dataset = dataset, cond = grid, border_plot = dataset_plot_border)


dataset_gen_unif <- function(size = 1000, nVar = 2, n_g = 2, class_fun = NULL, treshold = 0.5) 
  {
    
    # Verify if inputs are correct data types
    stopifnot("A numeric value needs to be provided for the size of the dataset" = is.numeric(size))
    stopifnot("A numeric value needs to be provided for the number of variables to be produced" = is.numeric(nVar))
    stopifnot("The classification function needs to be of the type function" = is.function(class_fun))
    stopifnot("Number of variables needs to be equal or above 2" = nVar >= 2)
    
    # Random sample of data
    sample <- replicate(nVar,stats::runif(size, min = 0, max = 10))
    sample <- dplyr::as_tibble(sample) %>% magrittr::set_colnames(paste0("x", 1:nVar))
    
    # Applies classification function if nVar = 2
    dataset <- sample %>% 
      mutate(
        g = purrr::pmap_dbl(., class_fun ),
        g = factor(g)
      )
    
    # Creates plot
    dataset_plot <- ggplot(dataset, aes(x1, x2, color = factor(g))) + 
      geom_point(size = 3, shape = 1) +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_blank(),
        legend.position="bottom",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.caption.position = "plot"
      ) +
      scale_colour_brewer(palette = "Set1")
    
  
    ## Build grid for contour
    x1_range <-  seq(0, 10, by = 0.05)
    x2_range <-  seq(0, 10, by = 0.05)
    grid <-  expand.grid(x1 = x1_range, x2 = x2_range)
    
    # conditional probability of (x1, x2) given y = 0
    grid <- grid %>% 
      mutate(
        g = purrr::pmap_dbl(., class_fun ),
        g = factor(g)
      )
    
    l <- list()
    
    for (i in 1:n_g) {
      
      l[[i]] <- ifelse(grid$g == i, 1, 0)
      
    }
    
    # Calculates conditional probabilities
    conditional_prb = do.call(cbind.data.frame, l) %>% 
      set_colnames(paste0("px_G",0:(n_g-1))) %>% 
      mutate(
        py0_x = treshold * px_G0,
        py1_x = (1-treshold) * px_G1,
        bayesborder = py1_x - py0_x ,
        predictclass = ifelse(py0_x > py1_x, 0, 1) # posterior class
      )
    
    
    
    
    
    grid <- cbind(grid, conditional_prb)
    
    dataset_plot_border <- dataset_plot +
      geom_contour(data = grid, aes(x = x1,y = x2, z = bayesborder), color = "black", linetype = "dashed", breaks = 0) 
    
    # return results
    results <- list(dataset_plot = dataset_plot, dataset = dataset, cond = grid, border_plot = dataset_plot_border)
    return(results)
  
  }




# Dataset gen with multivariated normal dist ------------------------------

#' @name: dataset generator with multivariated normal distribution
#'
#' @param 
#' @return 


dataset_gen_mvnorm <- function(l_mu, 
                               l_cvm,
                               l_w, 
                               size = 1000, 
                               nVar = 2, 
                               n_g = 2, 
                               class_fun = NULL, 
                               treshold = 0.5,
                               outlier_boost = NULL) {
  
  # generates samples
  
  l_sample <- list()
  
  for (i in 1:length(l_mu)) {
    
    s <- cbind(rmvnorm(size * l_w[[i]], l_mu[[i]], l_cvm[[i]]),i-1)
    l_sample[[i]] <- s
    
  }
  
  dataset <- do.call(rbind.data.frame,l_sample) %>% 
    magrittr::set_colnames( c(paste0("x", 1:nVar),"g" ) ) %>% 
    mutate(g = factor(g))
  
  # Creat outilers
  
  if (!is.null(outlier_boost)) {
    
    dataset <- dataset %>% 
      mutate_if( is.numeric, ~( ifelse(. > 8.5 | . < -0.5, . * outlier_boost, .)) )
    
  } else {
    
    dataset
    
  }
  
  
  # Creates plot
  dataset_plot <- ggplot(dataset, aes(x1, x2, color = factor(g))) + 
    geom_point(size = 3, shape = 1) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.line = element_blank(),
      legend.position="bottom",
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.caption.position = "plot"
    ) +
    scale_colour_brewer(palette = "Set1")
  
  
  ## Build grid for contour
  x1_range <-  seq(min(dataset$x1), max(dataset$x1), by = 0.05)
  x2_range <-  seq(min(dataset$x2), max(dataset$x2), by = 0.05)
  grid <-  expand.grid(x1 = x1_range, x2 = x2_range)
  
  # conditional probability of (x1, x2) given y = 0
  grid <- grid %>% 
    mutate(
      g = purrr::pmap_dbl(., class_fun ),
      g = factor(g)
    )
  
  grid_merge <- merge(grid, data.frame( class = 0:(n_g-1) ), all=TRUE)
  
  
  l <- list()
  
  for (i in 1:n_g) {
    
    l[[i]] <- ifelse(grid$g == i, 1, 0)
    
  }
  
 new_grid <- grid_merge %>% mutate(p_class = ifelse(class == g, 1, 0))
  
  
  # Calculates conditional probabilities
  conditional_prb = do.call(cbind.data.frame, l) %>% 
    set_colnames(paste0("px_G",0:(n_g-1))) %>% 
    mutate(
       py0_x = treshold * px_G0,
       py1_x = (1-treshold) * px_G1,
       bayesborder = py1_x - py0_x ,
       predictclass = ifelse(py0_x > py1_x, 0, 1) # posterior class
     )
  
  grid <- cbind(grid, conditional_prb)
  
  
  dataset_plot_border <- dataset_plot +
    geom_contour(data = grid, aes(x = x1, y = x2, z = bayesborder), color = "black", linetype = "dashed", breaks = 0)
    
  dataset_plot_border_newgrid <- dataset_plot +
    geom_contour(data = new_grid, aes(x = x1, y = x2, z = p_class, color = as.factor(class), group = as.factor(class)), bins = 1)
  
  
  # return results
  results <- list(dataset_plot = dataset_plot, dataset = dataset, cond = new_grid, border_plot = dataset_plot_border_newgrid)
  return( results )
  
}




# Classification metrics function -----------------------------------------

#' Calculate metrics for each model
#'
#' @param test_data A data frame
#' @param model A model
#' @return A list with fit, confusion matrix, confusion plot, accuracy, roc curve
#' and auc roc
#' 
#' @examples
#' 

model_metrics <- function(test_data = NULL, model = NULL )  {
  
      
  # Verify if inputs are correct data types
  stopifnot("A test dataframe should be provided" = is.data.frame(test_data))
  stopifnot("A model should be provided" = !is.null(model))
  
  
  # fit test data
  fit_test <- 
    test_data %>% 
    bind_cols(
      predict(model, new_data = test_data),
      predict(model, new_data = test_data, type = "prob"),
    ) %>% 
    mutate_if(is.numeric, round, digits= 3) %>% 
    mutate(
      decision = .pred_1 - .pred_0
    )
      
  # confusion matrix
  confusion_matrix <- conf_mat(fit_test, truth = g, estimate = .pred_class)
  confusion_matrix_plot <- autoplot(confusion_matrix, type = "heatmap")
  
  
  # Accuracy
  acc <- accuracy(fit_test, truth = g, estimate = .pred_class)
  
  # Roc curve
  roc_curve <- if (nlevels(test_data$g) > 2) {
  
    roc_curve(
      fit_test, truth = g, 
      paste0(".pred_",0):paste0(".pred_",nlevels(test_data$g)-1),
      .level = .pred_0) %>%  
      autoplot() 
  
  } else {
    
    roc_curve(fit_test, truth = g, estimate = .pred_0) %>% 
    autoplot()
  
  }
    
  
  auc_roc <- if (nlevels(test_data$g) > 2) {
    
    estimator = ifelse(nlevels(test_data$g) > 2, "macro_weighted",NULL) 
    
   roc_auc(fit_test,
            truth = g, 
            paste0(".pred_",0):paste0(".pred_",nlevels(test_data$g)-1),
            estimator = estimator)
    
    
  } else {
  
    roc_auc(fit_test, truth = g, .pred_0)
  
  }
  
  
  
  results <- list(fit = fit_test, 
                  cf_matrix = confusion_matrix, 
                  cf_plot =  confusion_matrix_plot, 
                  acc = acc, 
                  roc_curve = roc_curve, 
                  auc_roc = auc_roc)
  
  return(results)
}



# model_fit ---------------------------------------------------------------

#' Fits and compare two workflows applied to 2 datasets
#'
#' @param datasets A list of datasets to compare
#' @param workflows A list containing elements of type workflow
#' @param folds Integer number of folds for cross validation, default = 10
#' @return dsa
#' @example 

model_fit_compare <- function(data, workflows, folds = 10) {
  
  # Verify if inputs are correct data types
  stopifnot("Datasets should be a list of dataframes" = is.list(data))
  stopifnot("Workflows need to be of type workflow and passed as list" = is.list(workflows))
  
  # Define variables
  datasets <- lapply(data, function(f) f$dataset)
  grids <- lapply(data, function(f) f$cond)
  plots <- lapply(data, function(f) f$border_plot)
  
  names(datasets) <- paste0("dataset",1:length(datasets))
  names(grids) <- paste0("dataset",1:length(datasets))
  names(plots) <- paste0("dataset",1:length(datasets))
  
  
  # Split train and test
  splits <- lapply(datasets, rsample::initial_split, prop = 0.8)
  split_dataset <- list(
    train = lapply(splits, rsample::training),
    test = lapply(splits, rsample::testing)
  )
  
  
  # Fit control and resamples
  fit_control <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
  split_dataset$folds <- lapply(split_dataset$train, vfold_cv, v = folds)
  
  
  for (i in 1:length(split_dataset$folds)) {
    
    name = names(split_dataset$folds[i])
    
    # for metrics
    split_dataset$fit_resample_train[[name]] = 
      lapply(
        workflows, 
        tune::fit_resamples, 
        resamples = split_dataset[["folds"]][[i]], 
        verbose = TRUE, 
        control = fit_control
      )
    
    # models
    split_dataset$fit_model[[name]] =
      lapply(
        workflows, 
        parsnip::fit,
        split_dataset[["train"]][[i]]
      )
    
    # extract model
    split_dataset$models[[name]] =
      lapply(
        split_dataset$fit_model[[i]],
        workflows::extract_fit_parsnip
      )
  }

    
  ## Compare results
  
  # Define grids for plots contour
  for (i in 1:length(grids)) {
    
    for (m in 1:length(split_dataset$models)){
      grids$fitted[[paste0("dataset",i)]][[names(split_dataset$models[[i]][m])]] <- 
        grids[[i]] %>% 
        bind_cols(
          predict(split_dataset$models[[i]][[m]], new_data = grids[[i]]),
          predict(split_dataset$models[[i]][[m]], new_data = grids[[i]], type = "prob")
        ) %>% 
        mutate(
          .pred_1 = round(.pred_1, 3),
          .pred_0 = round(.pred_0, 3),
          decision = .pred_1 - .pred_0
        )
    }
  }
  
  # Generate new plots
  for (p in 1:length(plots)) {
    for (g in 1:length(grids$fitted)){
      plots$model_decision[[paste0("dataset",p)]][[names(grids$fitted[[p]][g])]] <- 
        plots[[p]] +
        geom_contour(data = grids$fitted[[p]][[g]], aes(x = x1,y = x2, z = decision), color = "Purple", breaks = 0, size = 1, alpha = 0.5) +
        geom_point(data = grids$fitted[[p]][[g]], aes(x = x1, y = x2, color =.pred_class ), size = 0.1, alpha = 0.1)
    }
  }
  
  results <- (list(plots = plots, models = split_dataset, grids = grids))
  return(results)
  
}



















