print.murphydiag <- function(x, ...) {
  gnorts <- names(x)[order_bymax(x)]
  attribs <- attributes(x)
  
  cat(sep = "\n",
    sprintf("murphydiag [1:%i] %s ...", length(x), paste(names(x)[1:6], collapse = " ")),
    sprintf("with '%s' functional predictions for %i observations\n", attribs$functional$type, length(attribs$y)),
    "Preliminary ranking by maximum value:",
    sprintf("%s < ... < %s", paste(head(gnorts, 3), collapse = " < "), paste(tail(gnorts, 3), collapse = " < "))
  )
}

order_bymax <- function(m) {
  max_values <- sapply(m, function(d) d$values$max)
  order(max_values)
}
