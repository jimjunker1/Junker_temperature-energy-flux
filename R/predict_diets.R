##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param full_diet_df
##' @param modeled_diets
predict_diets <- function(full_diet_df, modeled_diets) {

  diet_model = modeled_diets[["diet_model"]]
  
  model_predict = predict(diet_model, newdata = bind_rows(full_diet_df))

}
