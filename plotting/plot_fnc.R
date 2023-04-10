library(ggplot2)


#' Create a plot based on synthetic datasets
#'
#' @param syn.obj a list of synthetic datasets
#' @param lm.formula a linear model formula that user specify e.g., y ~ x
#'
#'
#' @return a ggplot object

synplot1 <- function(syn.obj, lm.formula){
  vars <- all.vars(lm.formula)
  # row-bind all dfs
  dplyr::bind_rows(!!!syn.obj, .id="id") %>% 
  # suppose lm.formula contains one predictor x
    ggplot(aes_string(x = vars[2], y = vars[1], color= "id")) +
    geom_point(size=1, alpha = 0.2) +
    geom_line(stat = "smooth", method = lm, alpha = 0.5, position = position_dodge(width = 1)) +
    theme_classic() +
    labs(title = paste("Linear Regression", deparse(lm.formula)), color="")
}




#' Create a plot of fitted values vs. observed values
#'
#' @param model.list a list of fitted model objects
#' @param smoother a smoothing method to use (default = "lm")
#'
#'
#' @return a ggplot object

synplot2 <- function(model.list, smoother = "lm"){
  # get the model formula
  form <- formula(model.list[[1]])
  model.list %>% 
    purrr::map(
      ~.x[c("fitted.values", "model", "residuals")] %>% 
        dplyr::bind_cols() %>% 
        # select fitted.values & DV which are the 1st and 2nd vars
        dplyr::select(1:2, last_col())
    ) %>% 
    dplyr::bind_rows(.id = "ids") %>% 
    dplyr::group_by(ids) %>% 
    # compute MSE per dataset
    dplyr::mutate(MSEs = mean(residuals^2), 
                  ids = glue::glue('{ids}, MSE= {round(MSEs, 2)}')) %>%
    dplyr::ungroup() %>% 
    {   # grab the dependent variable and unquote it
      ggplot(., aes(x = fitted.values,
                    y = !!rlang::sym(all.vars(form)[1]), 
                    
                    color= ids)) +
        geom_point(size = 1, alpha = 0.2) +
        geom_line(stat = "smooth", 
                  method = smoother, 
                  alpha = 0.5, 
                  #position = position_dodge(width = 1)
        ) +
        theme_classic() +
        labs(title = paste(smoother, "line for", deparse(form)), color="",
             subtitle = paste("Average MSE =", round(mean(.$residuals^2),2))) 
    }
}



library(magrittr)
library(mice)

#' Create a plot of average fitted values vs. observed values 
#' to show the variance between fitted values for the imputed datasets
#'
#' @param dat original data
#' @param imp.method imputation method (default = NULL)
#' @param lm.formula linear model formula
#' @param seed seed for mice (default = NA)
#' @param print print the output table (default = FALSE)
#'
#' @return a ggplot object

modelplot <- function(data, print=F){
  dv <- data[["call"]][["expr"]][[2]][[2]]
  
  tmp <- purrr::map_dfr(1:length(data$analyses), ~ {
    broom::augment(data$analyses[[.x]])
  }, .id="m") %>%
    ### extract row numbers :/ :/ :/ 
    dplyr::mutate(row = rep(1:(nrow(.)/dplyr::n_distinct(m)), dplyr::n_distinct(m))) %>% 
    tidyr::pivot_wider(id_cols = row, names_from = m, values_from = c(dv, .fitted, .resid)) %>% 
    dplyr::rowwise() %>% 
    dplyr::summarize(
      ### extract observed dat :/ :/ :/ 
      observed = ifelse(dplyr::n_distinct(c_across(starts_with(rlang::as_string(dv))))==1, get(paste0(dv,"_1")), NA),
      means = mean(c_across(starts_with(".fitted"))),
      vars = var(c_across(starts_with(".fitted")))
    )
  plot <- tmp %>% 
    ggplot2::ggplot(ggplot2::aes(x = means, y = observed, fill = vars, size = vars)) + 
    ggplot2::geom_point(alpha = 0.7, col = "black", shape=21)  +
    # reverse the col order
    #scale_color_distiller(palette = "YlOrRd", trans="reverse") +
    ggplot2::scale_fill_gradient(low =  "white", high = "#B61A51B3") +
    ggplot2::labs(title = paste("Model: ", deparse(data[["call"]][["expr"]])),
         x = "average fitted value", y = paste("observed", dv), color = "variance") +
    ggplot2::theme_classic() +
    # flip the color bar
    # guides(size = FALSE, col = guide_colourbar(reverse=T))  
    ggplot2::guides(size = "none")  
    
  # output table
  tab <- tmp %>% data.table::data.table()
  
  if(print) return(list(plot, tab)) else return(plot)
}



