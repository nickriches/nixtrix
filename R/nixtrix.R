#' Report p-value in APA style
#'
#' Uses "p < 0.001" if p-value less than 0.001. Otherwise outputs "p = 0.123" (note 3 decimal places)

#' @param p-value Numeric
#' @return A character vector, e.g. "p = 0.123"

p2apa <- function(x){
  if(x < 0.001) return("p < 0.001")
  if(x >= 0.001) return(paste0("p = ", round(x, 3)))
}

#' Report p-value in APA style and add stars
#'
#' Uses "p < 0.001" if p-value less than 0.001. Otherwise outputs "p = 0.123" (note 3 decimal places)
#' @param P-value Numeric
#' @return A character vector, e.g. "p = 0.023*"

p2apa_plus_stars <- function(x){
  if(x < 0.001) return("p < 0.001***")
  if(x >= 0.001) result = paste0("p = ", round(x, 3))
  if(x < 0.01) return(paste0(result, "**"))
  if(x < 0.05) return(paste0(result, "*"))
  return(result)
}

