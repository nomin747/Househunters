make_ui <- function(x, var) {
  if (is.numeric(x)) {
   rng <- range(x, na.rm = TRUE)
   sliderInput(var, var, min = rng[1], max = rng[2], round = TRUE, value = rng)
  } else if (is.character(x)) {
     levs <- unique(x)
     selectInput(var, var, choices = levs, 
                 #selected = levs, 
                 multiple = TRUE)
  } else {
     NULL
  }
  
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= (val[1]) & x <= (val[2])
  } else if (is.character(x)) {
    x %in% val
  } else {
    TRUE
  }
}