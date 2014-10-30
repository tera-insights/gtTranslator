.onAttach <- function(libname, pkgname) {
  grokit$libraries <- c(grokit$libraries, "translator")
  ## vars is a nest environment that replicates that replicates the nested block
  ## structure within C++ encapsulated by brackets.
  grokit$vars <- emptyenv()
  ## init is a character vector of the variables that are currently initialized
  ## and therefore which are usable outside of being assigned to. It is a character
  ## and not an environment like vars because vars retains enough information about
  ## the structure of the code blocks for the both of them.
  grokit$init <- character()
  ## the types defined in the "types" argument
  grokit$typedefs <- character()
  ## the constants defined in the "constants" argument
  grokit$constants <- list()
  ## template arguments that were required in constants
  grokit$templates <- character()
  ## fields is a character vector of the names of the fields of the class. It is
  ## used to check the validity of the $ operator calls.
  grokit$fields <- character()
  ## lists of templated types to be converted to C++ types
  grokit$types <- list()
  ## a list of attributes of types to be inserted into the code
  grokit$gets <- list()
  ## a list that contains information about casts performed, to be checked for compatibility
  ## a vector that tracks how many times a variable has been declared
  grokit$casts <- list()
  ## What type of return GetResult is translating. It is empty if not in GetResult.
  grokit$result <- ""
  ## a list used to keep track of multiple return statements in GetResult
  grokit$returns <- list()
  grokit$functions <- list(
      `[[` = list(
          '2' = '@1[@2]'),
      '+' = list(
          '1' = '+@1',
          '2' = '@1 + @2'),
      '%+=%' = list(
          '2' = '@1 += @2'),
      '-' = list(
          '1' = '-@1',
          '2' = '@1 - @2'),
      '%-=%' = list(
          '2' = '@1 -= @2'),
      '*' = list(
          '2' = '@1 * @2'), ## operator now used for matrix multiplication instead of %*%
      '%*=%' = list(
          '2' = '@1 *= @2'),
      '/' = list(
          '2' = '@1 / @2'), ## element wise matrix division and scalar divsion
      '%/=%' = list(
          '2' = '@1 /= @2'),
      '%%' = list(
          '2' = '@1 % @2'),
      '|' = list(
          '2' = '@1 || @2'),
      '&' = list(
          '2' = '@1 & @2'),
      '||' = list(
          '2' = '@1 || @2'),
      '&&' = list(
          '2' = '@1 && @2'),
      '<' = list(
          '2' = '@1 < @2'),
      '<=' = list(
          '2' = '@1 <= @2'),
      '>' = list(
          '2' = '@1 > @2'),
      '>=' = list(
          '2' = '@1 >= @2'),
      '==' = list(
          '2' = '@1 == @2'),
      '!=' = list(
          '2' = '@1 != @2'),
      '%*%' = list(
          '2' = '@1 % @2'),##operator now used for matrix element wise multipliaction
      '%.*%' = list(
          '2' = '@1 % @2'),##matlab operator
      '!' = list(
          '1' = '!@1'),
      'isTRUE' = list(
          '1' = '(bool) @1'),
      'crossprod' = list(
          '1' = 'trans(@1)*(@1)',
          '2' = 'trans(@1)*(@2)'),
      '^' = list(
          '2' = '@1 ^ @2'), ##works for both scalar and matrices
      '**' = list(
          '2' = 'pow(@1, @2)'),
      '%o%' = list(
          '2' = '@1 * trans(@2)'),
      't' = list(
          '1' = 'trans(@1)'),
      'solve' = list(
          '1' = 'inv(@1)', ##does not work on scalars like R does
          '2' = 'solve(@1, @2)',
          '3' = 'solve(@1, @2, @3)'),
      'crossprod' = list(
          '1' = 'trans(@1)*(@1)',
          '2' = 'trans(@1)*(@2)'),
      'exp' = list(
          '1' = 'exp(@1)'),
      'log' = list(
          '1' = 'log(@1)'),
      'abs' = list(
          '1' = 'abs(@1'),
      'sqrt' = list(
          '1' = 'sqrt(@1)'),
      'square' = list(
          '1' = 'square(@1)'),
      'ceiling' = list(
          '1' = 'ceil(@1)'),
      'floor' = list(
          '1' = 'floor(@1)'),
      'round' = list(
          '1' = 'round(@1)'),
      'cos' = list(
          '1' = 'cos(@1)'),
      'acos' = list(
          '1' = 'acos(@1)'),
      'cosh' = list(
          '1' = 'cosh(@1)'),
      'acosh' = list(
          '1' = 'acosh(@1)'),
      'sin' = list(
          '1' = 'sin(@1)'),
      'asin' = list(
          '1' = 'asin(@1)'),
      'sinh' = list(
          '1' = 'sinh(@1)'),
      'asinh' = list(
          '1' = 'asinh(@1)'),
      'tan' = list(
          '1' = 'tan(@1)'),
      'atan' = list(
          '1' = 'atan(@1)'),
      'tanh' = list(
          '1' = 'tanh(@1)'),
      'atanh' = list(
          '1' = 'atan(@1)'),
      'print' = list(
          '0' = 'std::cout << std::endl',
          '1' = 'std::cout << @1 << std::endl'),
      'typeof' = list(
          '1' = 'decltype(@1)')
      )
  grokit$typemap <- c(
      vector = "vec",
      matrix = "mat"
      )
}
