indent <- function(level = 1) {
  paste(rep("\t", level), collapse = NULL)
}

`%in%` <- function(x, table) {
  if (is.symbol(x))
    base::`%in%`(as.character(x), table)
  else
    base::`%in%`(x, table)
}

is.init <- function(names) {
  names <- unlist(lapply(names, as.character))
  names %in% grokit$init
}

is.declared <- function(names) {
  names <- unlist(lapply(names, as.character))
  !as.logical(lapply(mget(names, envir = grokit$vars, ifnotfound = 0, inherits = TRUE), identical, 0))
}

declare <- function(names) {
  names <- lapply(names, as.character)

  ## cannot re-declare variables
  if (any(bad <- is.declared(names)))
    stop("cannot re-declare variables: ", paste(names[bad], collapse = ", "))

  ## checks that names are syntactically correct in C++, i.e. only underscores and alpha-numeric
  if (length(bad <- grep("[^[:alnum:]_]", names)) != 0)
    stop("illegal local names: ", paste(names[bad], collapse = ", "))

  if (any(bad <- as.logical(lapply(names, is.type))))
    stop("type names used as variables: ", paste(names[bad], collapse = ", "))

  for (name in unlist(lapply(names, as.character)))
    grokit$vars[[name]] <- 1
}

init <- function(names) {
  for (name in unlist(lapply(names, as.character))) {
    if (!is.declared(name))
      declare(name)
    if (!is.init(name))
      grokit$init <- c(grokit$init, name)
  }
}

## There are no non-blocks allowed. This eases the process of maintaining
## environments and creates uniformity without sacrificing functionality.
as.block <- function(stmt) {
  if (stmt[[1]] != "{")
    call("{", stmt)
  else
    stmt
}

encode <- function(namespace) {
  paste0("@@", namespace, "@@")
}

start.block <- function() {
  grokit$vars <- new.env(parent = grokit$vars)
}

end.block <- function() {
  grokit$init <- grokit$init[!(grokit$init %in% ls(grokit$vars))]
  grokit$vars <- parent.env(grokit$vars)
}

is.template <- function(exprs) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  as.logical(lapply(exprs, is.language))
}

is.type <- function(exprs, template = TRUE) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  as.logical(lapply(exprs, function(expr) {
    ((is.template(expr) && template)
     || (is.symbol(expr) && as.character(expr) %in% grokit$typedefs)
    || (is.call(expr) && length(expr) == 2 && is.symbol(expr[[1]]) && expr[[1]] == "typeof"))
  }))
}

is.constant <- function(exprs) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  as.logical(lapply(exprs, function(expr) {
    (!is.language(expr)
     || is.symbol(expr)
     || is.getter(expr))
  }))
}

is.getter <- function(exprs) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  as.logical(lapply(exprs, function(expr) {
    (is.call.to(expr, "$")
     && is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% grokit$typedefs
     && is.symbol(expr[[3]]))
  }))
}
