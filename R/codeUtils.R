#' check if a call is an assignment
is.assignment = function(call) {
  if (length(call)==1)
    return(FALSE)
  
  char.op = as.character(call[[1]])
  char.op == "=" | char.op == "<-"
}

#' extracts from a call expression its variable and its index
extract.var.with.index = function(call, as.character=FALSE) {
  restore.point("extract.var.with.index")
  if (class(call)=="name") {
    var = call
    index = NULL
  } else {    
    if (call[[1]] == "[" | call[[1]] == "[[" | call[[1]] == "$") {
      var = call[[2]]
      index = call[[3]]
    }  else {
      var = call[[1]]
      index = NULL
    }
  }
  if (as.character) {
    var = as.character(var)
    if (!is.null(index))
      index = as.character(index)
  }
  list(var=var, index=index)
}


is.variable = function(call) {
  is.name(call)
}


#' get lhs of an assignment
get.lhs = function(call) {
  call[[2]]
}

#' get rhs of an assignment
get.rhs = function(call) {
  call[[3]]
}

examples.recursively.replace = function() {
  # remove indices from variables
  remove.index = function(call) {
    if (is.name(call)) return(call)
    if (as.character(call[[1]])=="[") {
      return(call[[2]])  
    }
    return(call)
  }
  eq = quote(K[t] <- I[t-1] +(1- delta) * K[t-1])
  static = recursively.replace(eq, remove.index)
  static
  
}

#' Recursively replace elements of a call or list
recursively.replace = function(call, replace.fun) {
  call = replace.fun(call)
  if (length(call)>1) {
    for (i in seq_along(call)) {
      call[[i]] = recursively.replace(call[[i]], replace.fun)
    }
  }
  call
}

#' Creates a call with name name and arguments in arg.list
make.call = function(name, arg.list, use.names=!is.null(names(arg.list))) {
  ca = call(name)
  if (length(arg.list)>0) {
    if (use.names) {
      ca[names(arg.list)] = arg.list
    } else {
      ca[2:(length(arg.list)+1)] = arg.list
    }
  }
  ca
}

#' substitutes in a call object x (works like substitute2 in pryr)
substitute.call <- function (x, env) 
{
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}
