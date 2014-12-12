MakeGLA <- function(types = list(), constants = list(), representation, prototype, AddItem, AddState, GetResult,
                    sys.headers = character(), user.headers = character(), lib.headers = character()) {
  ## After the call to this function exits, grokit will be effectively unchanged
  on.exit(.onAttach("", ""))
  start.block()

  ## declaring global typedefs
  types <- substitute(types)
  if (!(is.call.to(types, "list") && is.null(names(types)) && all(is.symbols(as.list(types)[-1]))))
    stop("`types` must be an un-named list specifying type names.")
  grokit$typedefs <- unlist(lapply(as.list(types)[-1], as.character))
  if (length(unique(grokit$typedefs)) != length(grokit$typedefs))
    stop("type names must be pairwise distinct.")
  if (is.null(grokit$typedefs))
    grokit$typedefs <- character()

  ## declaring constants
  constants <- substitute(constants)
  if (!(is.call.to(constants, "list") && all(is.constant(constants <- as.list(constants)[-1]))
        && (length(constants) == 0 || (!is.null(names(constants)) && all(names(constants) != "")))))
    stop("`constants` must be a named list specifying constant values.")
  if (any(bad <- names(constants) %in% grokit$typedefs))
    stop("nameclashes between `constants` and `types`: ", names(constants)[bad])
  if (is.null(constants))
    grokit$constants <- character()
  else
    grokit$constants <- names(constants)
  if (length(unique(grokit$constants)) != length(grokit$constants))
    stop("constant names must be pairwise distinct.")
  init(grokit$constants)
  constants <- Convert.Constants(constants)
  names(constants) <- grokit$constants

  ## declaring fields of the class
  representation <- substitute(representation)
  if (!(is.call(representation) && is.symbol(representation[[1]]) && representation[[1]] == "list"))
    stop("representation must be a field => type list.")
  representation <- as.list(representation)[-1]
  if (is.null(names(representation)) || any(names(representation) == ""))
    stop("representation must have a name for each element.")
  if (any(bad <- duplicated(names(representation))))
    stop("cannot have repeated class variable names in representation: ",
         paste(names(representation)[bad], collapse = ", "))
  if (any(bad <- !is.type(representation)))
    stop("types improperly specified: ", paste(lapply(representation[bad], deparse), collapse = ", "))
  ## this actually gets passed through to PHP with names => type
  fields <- as.list(Convert.Type(representation))
  ## this is just used in R and only contains the names of the fields as no type-checking is done
  grokit$fields <- names(representation)
  declare(grokit$fields)

  ## Enforcing that the expressions for the three functions at least evaluate to
  ## functions in R, although they could possibly be specified non-literally.
  if (!is.function(prototype))
    stop("prototype must be a function.")
  if (!is.function(AddItem))
    stop("AddItem must be a function.")
  if (!is.function(AddState))
    stop("AddState must be a function.")
  if (!is.function(GetResult))
    stop("GetResult must be a function.")

  prototype <- substitute(prototype)
  AddItem <- substitute(AddItem)
  AddState <- substitute(AddState)
  GetResult <- substitute(GetResult)

  ## This allows the user to use non-literal expressions, such as a function
  ## previously defined. Source code element will be different though.
  if (prototype[[1]] != "function")
    prototype <- parse(text = deparse(eval(prototype)))[[1]]
  if (AddItem[[1]] != "function")
    AddItem <- parse(text = deparse(eval(AddItem)))[[1]]
  if (AddState[[1]] != "function")
    AddState <- parse(text = deparse(eval(AddState)))[[1]]
  if (GetResult[[1]] != "function")
    GetResult <- parse(text = deparse(eval(GetResult)))[[1]]

  initialized <- as.list(prototype[[2]])
  if (any(initialized == ""))
    stop("arguments to prototype must be default values.")
  init(names(initialized))
  initialized <- lapply(initialized, Convert.Expr)
  constructor <- Convert.Function(prototype)

  start.block()
  params <- as.list(AddItem[[2]])
  init(names(params))
  params[params == ""] <- list(NULL)
  if (any(bad <- !is.type(params) && !(nulls <- as.logical(lapply(params, is.null)))))
    stop("improperly specified types in AddItem: ", paste(lapply(params[bad], deparse), collapse = ", "))
  arguments <- names(params)
  params <- ifelse(is.type(params, FALSE) | as.logical(lapply(params, is.null)),
                   lapply(params, Convert.Type), lapply(params, Convert.Template))
  names(params) <- arguments
  AddItem <- Convert.Function(AddItem)
  end.block()

  start.block()
  grokit$other <- names(AddState[[2]])
  if (length(grokit$other) != 1)
    stop("AddState must have exactly one argument.")
  init(grokit$other)
  AddState <- Convert.Function(AddState)
  other <- grokit$other
  grokit$other <- NULL
  end.block()

  start.block()
  grokit$result <- TRUE
  header <- as.list(GetResult[[2]]) ## from pairlist to list
  if (any(bad <- (header == "") | !is.template(header)))
    stop("GetResult: ", names(header)[bad], " were not given types as default arguments.")
  if (any(tolower(unlist(lapply(header, deparse))) %in% c("json", "base::json")))
    if (length(header) > 1)
      stop("GetResult was given extraneous arguments for returning a JSON.")
    else
      grokit$return <- "json"
  else
    grokit$return <- "tuple"
  grokit$header <- lapply(header, Convert.Template)
  GetResult <- Convert.Function(GetResult)
  end.block()

  ## exits out of fields
  end.block()

  sys_headers <- lapply(sys.headers, as.character)
  user_headers <- lapply(user.headers, as.character)
  lib_headers <- lapply(lib.headers, as.character)

  gla <- function(data, ..., inputs = AUTO, outputs = DUMMY, force.frame = FALSE) {
    templates <- list(...)
    required <- dummy
    if (any(bad <- !(required %in% names(templates))))
      stop("arguments required but not given: ", paste(required[bad], collapse = ", "))

    inputs <- substitute(inputs)
    check.exprs(inputs)
    if (is.auto(inputs))
      inputs <- convert.schema(x$schema)
    atts <- DUMMY
    inputs <- convert.exprs(inputs, data, atts = atts)

    outputs <- substitute(outputs)
    check.atts(outputs)
    if (is.auto(outputs))
      stop("outputs not allowed to be AUTO.")
    outputs <- convert.atts(outputs)
    l <- DUMMY
    if (length(outputs) != l)
      stop("There must be exactly ", l, "output", if (l > 1) "s", " specified.")

    gla <- DUMMY
    agg <- Aggregate(data, gla, inputs, outputs)
  }

  formals(gla)[[3]] <- as.call(c(as.symbol("c"), lapply(names(header), as.symbol)))

  body(gla)[[3]][[3]] <- as.call(c(as.symbol("c"), grokit$templates))
  body(gla)[[8]][[3]] <- as.call(c(as.symbol("c"), names(params)))
  body(gla)[[14]][[3]] <- length(header)
  body(gla)[[16]][[3]] <- call("GLA",
                               substitute(translator::UserDefined),
                               initialized = initialized,
                               constructor = constructor,
                               AddItem = AddItem,
                               AddState = AddState,
                               GetResult = GetResult,
                               fields = fields,
                               constants = constants,
                               params = params,
                               other = other,
                               typedefs = as.list(grokit$typedefs),
                               types = grokit$types,
                               gets = grokit$gets,
                               result = grokit$results,
                               return = grokit$return,
                               header = grokit$header,
                               templates = substitute(templates),
                               sys.headers = sys.headers,
                               user.headers = user.headers,
                               lib.headers = lib.headers)
  gla
}
