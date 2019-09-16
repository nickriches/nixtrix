
get_optional_argument <- function(x){
  if(missing(x)) return(NULL)
  if(!missing(x)) return(x)
}


#' Report p-value in APA style
#'
#' Uses "p < 0.001" if p-value less than 0.001. Otherwise outputs "p = 0.123" (note 3 decimal places)
#' stars = TRUE option adds stars where appropriate.

#' @param p-value Numeric
#' @param stars Optional argument. Logical (TRUE/FALSE)
#' @return A character vector, e.g. "p = 0.123"

p2apa <- function(p, stars){
  if(p < 0.001) pstring = "p < 0.001"
  if(p >= 0.001) pstring = paste0("p = ", round(p, 3))
  if(missing(stars)) return(pstring)
  if(!missing(stars)){
  starstring <- ""
  if(stars == TRUE & p < 0.001) starstring = "***"
  if(stars == TRUE & p < 0.01) starstring = "**"
  if(stars == TRUE & p < 0.05) starstring = "*"
  return(paste0(pstring, starstring))
  }
}

#' Report output of cor.test in APA style
#'
#' @param model Output of corr.test function
#' @param stars Optional argument. logical = TRUE if stars are required for p-values
#' @return A character vector, e.g. "r(12) = 0.12, p = 0.123"
#'

corr.test2apa <- function(x, stars){
    return(paste0("r(", x$parameter, ") = ", round(x$estimate, 2), ", ",
                  p2apa(x$p.value, stars)))
  }





