Convert.Function <- function(fun) {
  Convert.Stmt(as.block(fun[[3]]))
}

## This function only deuces what kind of statement the expression is, such as
## an assignment, a for loop, a conditional, etc. It then calls the appropiate
## translator. This strategy is used to better section the code and facilitates
## future additions to it.
Convert.Stmt <- function(expr) {
  statement <- switch(
      typeof(expr),
      logical =,
      numeric =,
      integer =,
      symbol = {
        Warning("constant statement on line ", grokit$line, " ignored.")
        NULL ## Warning has invisible return
      },
      list = {
        Stop("a list appeared on line ", grokit$line, ". Please report this.")
      },
      language = {
        switch(
            expr[[1]], ## keywords and special functions go here
            "<-" = Convert.Declaration(expr),
            "=" = Convert.Assignment(expr),
            "<<-" = Stop("only <- and = are allowed for assignment."),
            "for" = Convert.For(expr),
            "repeat" = Convert.Loop(expr),
            "while" = Convert.Loop(expr),
            "if" = Convert.If(expr),
            "break" = "break",
            "next" = "continue",
            switch = Convert.Switch(expr),
            locals = Convert.Locals(expr),
            return = Convert.Return(expr),
            "{" = {
              start.block()
              translation <-
                paste("{",
                      paste0(lapply(expr[-1], Convert.Stmt), ";",
                             collapse = "\n"),
                      "}", sep = "\n")
              end.block()
              translation
            },
            ## Semi-colons are added here and only here. This strategy
            ## guarantees them to appear at the end of every statement,
            ## even a FOR loop without brackets. There will be extra semi-
            ## colons after every closing bracket within a function body.
            Convert.Expr(expr)
            )
      },
      Stop("unexpected expr with type ", typeof(expr), ":\n", deparse(expr))
      )
  statement
}

Convert.Return <- function(expr) {
  if (grokit$return == "json") {
    result <- as.list(expr)[-1]
    names <- names(result)
    if (is.null(names) || any(names == ""))
      Stop("return for JSON must be given argument names: ", deparse(expr))
    values <- lapply(result, Convert.Expr)
    grokit$results <- c(grokit$results, list(values))
    ## subtract 1 to account for PHP arrays starting at 0
    encode(paste0("result", length(grokit$results) - 1))
  } else if (grokit$return == "tuple") {
    result <- as.list(expr)[-1]
    if (!is.null(names(result)))
      Stop("return for tuple should not be given argument names: ", deparse(expr))
    if (length(grokit$header) != length(result))
      Stop("expected ", length(grokit$header), " arguments to return: ", deparse(expr))
    values <- lapply(result, Convert.Expr)
    grokit$results <- c(grokit$results, list(values))
    ## subtract 1 to account for PHP arrays starting at 0
    encode(paste0("result", length(grokit$results) - 1))
  } else if (grokit$result == "") {
    if (length(expr) > 2)
      Stop("return given more than 1 argument.")
    paste("return", Convert.Expr(expr[[2]]))
  }
}

Convert.Declaration <- function(expr) {
  target <- expr[[2]]
  value <- expr[[3]]
  if (is.symbol(target)) {
    if (is.declared(target))
      Stop("cannot assign a type to a previously delcared variable: ", deparse(expr))
    if (!is.type(value))
      Stop("type improperly specified: ", deparse(expr))
    declare(target)
    paste(Convert.Type(value), Convert.Ident(target, FALSE))
  } else {
    ## This seems to hold true. Slight possibility to be changed in the future.
    Stop("type assignment only works on basic variables: ", deparse(expr))
  }
}


## AUTO is used in the case of a new variable being assigned before being
## declared. This puts the burden on the user to remember what type it is.
Convert.Assignment <- function(expr) {
  target <- expr[[2]]
  value <- expr[[3]]
  if (is.symbol(target)) {
    if (!is.declared(target)) {
      init(target)
      lhs <- paste("auto", Convert.Ident(target))
      rhs <- Convert.Expr(value)
    } else {
      init(target)
      lhs <- Convert.Ident(target)
      rhs <- Convert.Expr(value)
    }
  } else {
    lhs <- Convert.Expr(target)
    rhs <- Convert.Expr(value)
  }
  paste(lhs, "=", rhs)
}

## There is a brief attempt to produce a for-loop out of a numeric vector.
## Otherwise, a for each loop is made. So far the only attempt made is to
## translate a:b, in which a and b are arbitrary expressions.
Convert.For <- function(expr) {
  index <- expr[[2]]
  if (!is.symbol(index))
    Stop("loop index is somehow not a symbol. Please report this.")
  assign(index, "AUTO")

  ## Translating sequence
  if (x[[3]][[1]] == ":") {
    assignment <- paste(Convert.Ident(index), "=", Convert.Expr(x[[3]][[2]]))
    condition1 <- paste(Convert.Ident(index), "<=", Convert.Expr(x[[3]][[3]]))
    condition2 <- paste(Convert.Ident(index), "<=", Convert.Expr(x[[3]][[3]]))
    increment1 <- paste0(Convert.Ident(index), "++")
    increment2 <- paste0(Convert.Ident(index), "--")
  }

  body <- Convert.Body(expr[[4]])

  paste0("for (", header, ")", body)
}

Convert.Loop <- function(expr) {
  if (expr[[1]] == "while") {
    condition <- Convert.Expr(expr[[2]])
    body <- expr[[3]]
  } else {
    condition <- "true"
    body <- expr[[2]]
  }
  body <- Convert.Stmt(as.block(body))

  paste0("while (", condition, ") ", body)
}

Convert.If <- function(expr) {
  condition <- Convert.Expr(expr[[2]])
  true <- Convert.Stmt(as.block(expr[[3]]))
  if (length(expr) == 4)
    false <- paste("else", Convert.Stmt(as.block(expr[[4]])))
  else
    false <- NULL
  paste0("if (", condition, ")", true, false)
}

Convert.Locals <- function(expr) {
  types <- as.list(expr)[-1]
  locals <- names(types)

  if (any(bad <- is.declared(locals)))
    Stop("`locals` used to re-declare a variable: ", paste(locals[bad], collapse = ", "))
  if (any(bad <- !is.type(types)))
    Stop("types in `locals` specified incorrectly:\n", paste("\t", lapply(types[bad], deparse), collapse = ",\n"))

  types <- lapply(types, Convert.Type)
  paste(Convert.Type, locals, collapse = ";\n")
}

Convert.Expr <- function(expr) {
  if (is.symbol(expr)) {
    Convert.Ident(expr)
  } else if (is.call(expr)) {
    if (is.call.to(expr, "$")) {
      if (is.symbol(expr[[2]]) && !is.null(grokit$other) && grokit$other == expr[[2]]) {
        ## Case of field in other state
        if (!(is.symbol(expr[[3]]) && as.character(expr[[3]]) %in% grokit$fields))
          Stop("when calling `$` on `other`, the second argument should be a field: ", deparse(expr))
        paste0(Convert.Expr(expr[[2]]), ".", Convert.Expr(expr[[3]]))
        ## Case of get(PHPtype, attribbute)
      } else if (is.getter(expr)) {
        Stop("type querying is only allowed in `constants`: ", deparse(expr))
      } else {
        Stop("incorrect call to `$`: ", deparse(expr));
      }
    } else {
      fun <- Convert.Call(expr[[1]])
      args <- lapply(as.list(expr)[-1], Convert.Expr)
      if (fun %in% names(grokit$functions)) {
        ## Special pattern for formatting purposes or just re-naming a function.
        if (length(args) %in% names(grokit$functions[[fun]])) {
          pattern <- grokit$functions[[fun]][[as.character(length(args))]]
          for (i in length(args):1)
            pattern <- gsub(paste0("@", i), args[[i]], pattern)
          pattern
        } else {
          Stop("operator ", fun, " cannot take ", length(args), "arguments.\n",
               "expr: ", deparse(expr))
        }
        ## The following are basic cases of other operators.
      } else if (fun %in% c("<-", "<<-")) {
        Stop("nested assignment is dis-allowed.")
      } else if (fun == ":") {
        Stop("sequence operator is allowed only in for loops.")
      } else if (fun == "[") {
        Stop("template arguments are only allowed for function calls.")
      } else if (fun %in% c("c", "list")) {
        Stop("combining function is dis-allowed.")
      } else if (fun == "return") {
        Stop("return only allowed at the top-most level of statements.")
      } else if (fun == "{") {
        paste0("{", paste(args, collapse = ", "), "}")
      } else if (fun == "(") {
        paste0("(", paste(args, collapse = ", "), ")")
      } else if (substr(fun, 1, 3) == "as.") {
        ## Casting
        fun <- expr[[1]]
        if (!is.symbol(fun))
          Stop("type conversion function formatted incorrectly.")
        if (length(args) != 1)
          Stop("type casting takes a single argument.")
        paste(paste0("(", substring(as.character(fun), 4), ")"), Convert.Expr(expr[[2]]))
      } else {
        ## Basic call
        paste0(fun, "(", paste(args, collapse = ", "), ")")
      }
    }
  } else if (is.numeric(expr)) {
    if (is.double(expr) && is.whole(expr))
      paste0(as.character(expr), ".0")
    else
      as.character(expr)
  } else if (is.character(expr)) {
    paste0('"', expr, '"')
  } else if (is.logical(expr)) {
    if (expr) "true" else "false"
  } else {
    Stop("unexpected language structure. Please report this.")
  }
}

Convert.Call <- function(expr) {
  if (is.symbol(expr))
    as.character(expr)
  else if (is.call.to(expr, "::"))
    paste0(Convert.Call(expr[[2]]), "::", Convert.Call(expr[[3]]))
  else if (is.call.to(expr, "$"))
    paste0(Convert.Call(expr[[2]]), ".", Convert.Call(expr[[3]]))
  else if (is.call.to(expr, "$"))
    paste0(Convert.Call(expr[[2]]), "<",
           if (length(expr) > 2) paste(lapply(as.list(expr)[-(1:2)], Convert.Expr), collapse = ", "),
           ">")
  else if (is.call(expr))
    paste0(Convert.Call(expr[[1]]), "(",
           paste(lapply(as.list(expr)[-1], Convert.Expr), collapse = ", "),
           ")")
  else
    Stop("invalid call to a function: ", deparse(expr))
}

## value is true if the identifier is treated as a value and not an lvalue.
## Checking for initialization is done only if value is true.
Convert.Ident <- function(exprs, value = TRUE) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  unlist(lapply(exprs, function(expr) {
    if (!is.declared(expr))
      Stop("undeclared variable: ", expr)
    if (is.declared(expr) && value && !is.init(expr))
      Stop("uninitialized variable used: ", expr)
    as.character(expr)
  }))
}

Convert.Constants <- function(exprs) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  lapply(exprs, function(expr) {
    if (is.getter(expr)) {
      Convert.Getter(expr)
    } else if (is.symbol(expr)) { ## template argument
      grokit$templates <- c(grokit$templates, as.character(expr))
      as.character(expr)
    } else if (!is.language(expr)) { ## constant value
      Convert.Expr(expr)
    } else {
      Stop("invalid specification of constant: ", deparse(expr))
    }
  })
}

Convert.Type <- function(exprs) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  unlist(lapply(exprs, function(expr) {
    if (is.call.to(expr, "typeof")) {
      Convert.Expr(expr)
    } else if (is.symbol(expr) && as.character(expr) %in% grokit$typedefs) {
      as.character(expr)
    } else if (is.template(expr)) {
      grokit$types <- c(grokit$types, list(Convert.Template(expr)))
      encode(paste0("type", length(grokit$types) - 1))
    } else {
      NULL
    }
  }))
}

Convert.Template <- function(expr) {
  if (is.call(expr))
    c("_name" = Convert.TypeName(expr[[1]]), lapply(as.list(expr)[-1], Convert.Template))
  else if (is.symbol(expr) && !(as.character(expr) %in% c(grokit$constants, grokit$typdefs)))
    list("_name" = Convert.TypeName(expr))
  else
    expr
}

Convert.TypeName <- function(expr) {
  if (is.call(expr)) {
    if (!is.call.to(expr, "::") && is.symbols(as.list(expr)))
      Stop("type name specified incorrectly: ", deparse(expr))
    paste0(expr[[2]], "::", expr[[3]])
  } else if (is.symbol(expr)) {
    paste0("base::", expr)
  } else {
    Stop("type name specified incorrectly: ", deparse(expr))
  }
}

Convert.Getter <- function(expr) {
  if (!is.symbol(expr[[3]]))
    Stop("when calling `$` on a type from `types`, the second argument must be a symbol: ", deparse(expr))
  add <- list(which(grokit$typedefs == as.character(expr[[2]])) - 1)
  names(add) <- as.character(expr[[3]])
  add
}
