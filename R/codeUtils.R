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

examples.find.variables = function() {
  find.variables(quote({x = y^2-z+max(a,5)}))
}

#' Find all variables from a call or expression object
#' 
#' @return unique variables names as character vector
find.variables = function(call, max.level=Inf, level=1) {
  if (level > max.level) return(NULL)  
  if (is.name(call)) return(as.character(call))
  if (length(call)<=1) return(NULL)  
  names = lapply(call[-1], function(e1) {
    find.variables(e1, max.level=max.level, level=level+1)
  })
  names = unique(unlist(names, use.names=FALSE))
  names
}

examples.find.funs = function() {
 
  call = quote({
    mutate(group_by(x,type), Q=sum(q))
    .Globalenv
    print("y")
    5*3
  })
  find.funs(call)
  find.funs(call, max.level=2)
  find.variables(call)
}

#' Find all globale variables in a function
#' 
#' just a wrapper to codetools::findGlobals
find.global.vars = function(fun) {
  codetools::findGlobals(fun,merge=FALSE)$variables
}

#' Find all function calls from a call or expression object
#' 
#' @return unique names of called functions as character vector
find.funs = function(call, max.level=Inf, level=1) {
  if (level > max.level) return(NULL)
  if (!is.call(call)) return(NULL)
  fun.name = as.character(call[1])
  sub.names = lapply(call[-1], function(e1) {
    find.funs(e1, max.level=max.level, level=level+1)
  })
  names = unique(c(fun.name,unlist(sub.names, use.names=FALSE)))
  names
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

examples.subst.var = function() {
  call = "x+y==f(x)"
  substitute.var(call, "x","(x+3)")
  
}

parse.as.call = function(text) {
  parse(text=text)[[1]]
}

#' Substitute a variable or a symbol in the expression by subs
#' @param call a call object or string
#' @param var a symbol or string
#' @param subs a call or string
#' @export
subst.var <- function(call, var, subs, subset=TRUE) {
  restore.point("substitute.variable")
  if (!is.character(var)) var = deparse(var)
  if (is.character(call)) call = parse(text=call)[[1]]
  if (is.character(subs)) subs = parse(text=subs)[[1]]
  
  sub.li = list(subs)
  names(sub.li) = var
  
  res = substitute.call(call, sub.li)
  if (subset) res = res[[1]]
  res
}