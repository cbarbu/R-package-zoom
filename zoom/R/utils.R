## test return value of "try"
isError <- function(x) {
  return(class(x) == "try-error")
}

