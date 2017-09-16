
#' \code{getName} -- Get the name of a life table
#'
#' @param object A life table object
#'
#' @return the value of the slot "name" of the lifetable.
#' @export
#' @seealso \code{\link{TableMort}}
#' @examples
#' getName(th002d)
#' getName(tf002v)
#' getName(expolaw)
setGeneric(name = "getName", function(object) {
    standardGeneric("getName")
})


#' \code{getTable} -- Get the data.frame from a discrete life table
#' 
#' This method gets you the data.frame coresponding to the life table passed as the \code{object} parameter. Be carefull, because life tables could be defined in a continuous way, and in this case this method wont work, but you could rely on the underlying cdf that you can get via \code{\link{getCdf}}.
#'
#' @inheritParams getName
#'
#' @return a data.frame with to collumns -- One representing the ages ans an other one representing the Lx's
#' @export
#' @seealso \code{\link{Lx}}, \code{\link{getAgeMax}}, \code{\linkS4class{TableMort}}, \code{\link{getCdf}}
#' @examples
#' getTable(th002d)
#' getTable(tf002v)
#' \dontrun{
#' getTable(expolaw)
#' }

setGeneric(name = "getTable", function(object) {
    standardGeneric("getTable")
})

#' \code{getAgemax} -- Get the maximum age omega of a discrete life table
#' 
#' This method gets you the maximum age from a given life table. The life table have to be discrete. 
#'
#' @inheritParams getName
#'
#' @return a numeric coreponding to maximum ages that insuree could be.
#' @export
#' @seealso \code{\link{Lx}}, \code{\link{getAgeMax}}, \code{\linkS4class{TableMortDiscrete}}, \code{\link{getTable}}
#' @examples
#' getAgeMax(th002d)
#' getAgeMax(tableGPDId)
setGeneric(name = "getAgeMax", function(object) {
    standardGeneric("getAgeMax")
})

#' \code{getCdf} -- Get the Cdf from a continous life table
#' 
#' This method gets you the cdf coresponding to the life table passed as the \code{object} parameter. Be carefull, because life tables could be defined in a discrete way, and in this case this method wont work, but you could rely on the underlying data.frame that you can get via \code{\link{getTable}}.
#'
#' @inheritParams getName
#'
#' @return a function coresponding to the CDF that defined the life table. If you'r life table is multidimentional, so will be the CDF.
#' @export
#' @seealso \code{\link{Lx}}, \code{\link{getAgeMax}}, \code{\linkS4class{TableMortContinue}}, \code{\link{getTable}}
#' @examples
#' getCdf(expolaw)(10)
#' getCdf(expolaw2)(30)
#' \dontrun{
#' getCdf(th002d)(5)
#' }
setGeneric(name = "getCdf", function(object, ...) {
    standardGeneric("getCdf")
})


#' \code{Lx} -- Get the number of living people at some age.
#' 
#' This method gets you the maximum age from a given life table. The life table have to be discrete. 
#' If UDD is given, the method can use inter-ages \code{x} paramters. This method is fully vectorized, as you can see in following exemples. 
#'
#' @param table A life table object
#' @param x A numerical age (or a vector or a matrix) representing the age of the person.
#' @param UDD A boolean -- Should UDD been used ?
#' 
#' 
#'
#' @return a numeric coreponding to maximum ages that insuree could be.
#' @export
#' @seealso \code{\link{Lx}}, \code{\link{getAgeMax}}, \code{\linkS4class{TableMortDiscrete}}, \code{\link{getTable}}
#' @examples
#' Lx(expolaw, 12.7)
#' Lx(expolaw2, 12.7)
#' Lx(th002d, 12.7)
#' Lx(th002v, 12, UDD = FALSE)
#' Lx(tf002v, 12.7, UDD = TRUE)
#' Lx(tf002d, 12.7, UDD = TRUE)
#' 
#' #The Lx method is properly vectorised as follows : 
#' Lx(tableGPDId, c(1, 2, 3, 4))
#' Lx(tableGPDId, c(1, 2, 3, 4))
#' Lx(tableGPDId, matrix(1:12, nrow = 4))
#' Lx(tableGPDId, cbind(c(1, 2, 3, 4), c(1, 3, 4, 5), c(1, 5, 7, 9)))
#' 
#' 
setGeneric(name = "Lx", function(table, x, UDD = TRUE) {
    standardGeneric("Lx")
})


#' @rdname pxt
setGeneric(name = "qxt", function(table, x, t = 1) {
    standardGeneric("qxt")
})


#' \code{pxt}-- Probabilities of survival and death.
#' 
#' The two methods \code{qxt} and \code{pxt} allow us to get probabilities of survival and death from life tables.
#' Note that \code{qxt} is just a wrapper for 1 - pxt.  
#' 
#' @param table A \code{\link{TableMort}} object.
#' @param x A numerical age (or a vector or a matrix) representing the age of the person
#' @param t Defaulted to 1, represent the time that you want to person to survive (for pxt) or to die in (for qxt)
#' 
#' 
#' @return A numeric between 0 and 1, the probability of the event that a person age \code{x}, given the life table \code{table} survives for \code{t} years at least (\code{pxt}) or dies in the next \code{t} years (\code{qxt}).
#' @export
#' @seealso \code{\link{Lx}}, \code{\link{getAgeMax}}, \code{\linkS4class{TableMort}}, \code{\link{getTable}}
#' @examples
#' 
#' pxt(tableGPDIc, x = c(1, 2, 3, 4), t = c(1, 2, 3))
#' pxt(tableGPDIc, x = c(1, 2, 3, 4), t = c(1, 2, 3))
#' pxt(tableGPDIc, x = cbind(c(3, 4, 5, 6), c(1, 2, 3, 4)), t = c(1, 2, 3))
#' pxt(tableGPDId, x = c(1, 2, 3, 4), t = c(1, 2, 3))
#' pxt(tableGPDId, x = cbind(c(3, 4, 5, 6), c(1, 2, 3, 4)), t = c(1, 2, 3))
#'
#' # Test the sum -- Should be one : 
#' pxt(th002d,1,5) + qxt(th002d,1,5) = 1
setGeneric(name = "pxt", function(table, x, t = 1) {
    standardGeneric("pxt")
})

setGeneric(name = "Ext", function(table, x, t = 1) {
    standardGeneric("Ext")
})

setGeneric(name = "ax", function(table,
                                 x,
                                 temp = +Inf,
                                 diff = 0,
                                 m = 1,
                                 due = TRUE,
                                 continue = FALSE) {
    standardGeneric("ax")
})

setGeneric(name = "Ax", function(table, x, temp = +Inf,
                                 diff = 0, m = 1, continue = FALSE) {
    standardGeneric("Ax")
})

setGeneric(name = "ex", function(table, x = 0, continue = FALSE) {
    standardGeneric("ex")
})

setGeneric(name = "muxt", function(table, x, t = 0) {
    standardGeneric("muxt")
})

setGeneric(name = "is.cont", function(.Object) {
    standardGeneric("is.cont")
})

setGeneric(name = "SkTables", function(table, k) {
    standardGeneric("SkTables")
})

setGeneric(name = "SN", function(table, f, x, t, paA) {
    standardGeneric("SN")
})

setGeneric(name = "isOk", function(table, x) {
    standardGeneric("isOk")
})

setGeneric(name = "getSexe", function(object) {
    standardGeneric("getSexe")
})

setGeneric(name = "setSexe", function(object, Sexe) {
    standardGeneric("setSexe")
})

setGeneric(name = "getAge", function(object) {
    standardGeneric("getAge")
})

setGeneric(name = "setAge", function(object, Age) {
    standardGeneric("setAge")
})

