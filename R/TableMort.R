#' @include Generics.R Verbosity.R
NULL


#' \code{TableMort} - Mortality tables virtual motherclass.
#'
#' This class is a virtual class that's used to bind together all mortality table classes ( Continuous, discretes, groups...) that come with \code{actulife} package.
#' The Life tables classes that are availiables are \code{\link{TableMortDiscrete}}, \code{\link{TableMortContinue}}, \code{\link{TableMortGroupePDI}}, \code{\link{TableMortGroupePDIDiscrete}} and \code{\link{TableMortGroupePDIContinue}}. Depending on the kind of life table that you'r working with, you might use the Discrete's ones for discrete (whole year) life table an calculations, or the COntinuous one's for continuous life tables or calculations. You might also use the GroupePDI ( wich stands for "First death independant group") if you'r calculations deals with more than one life. Last but not least, the class \code{\link{Assure}} uses all of them in a nice and userfriendly approach. If you dont know how to start, you should go there.   
#' The following methods are availiable for all mortality tables as member of this motherclass : 
#' 
#' \code{\link{getName}} -- Simple method to get the name of the mortality table.
#' \code{\link{pxt}} -- Probability of survival
#' \code{\link{qxt}} -- Probability of death
#' \code{\link{Ext}} -- Pure endowment
#' \code{\link{ex}} -- Expectations of life (curtate or not)
#' \code{\link{ax}} -- anuities (due, imediate, whole life or not, EoY, EoM, continuous, etc.)
#' \code{\link{Ax}} -- Insurences (same parametrisations as anuities)
#' \code{\link{summary}} -- Gives you a summary of the life table
#'
#' @name TableMort
#' @export
setClass(Class = "TableMort",
         representation = representation(name = "character"))


setMethod("getName", "TableMort", function(object) {
    return(object@name)
})  # Accesseur du nom de la table. TRANSCRIT
##### Methodes et surcharges de methodes.

setMethod("qxt", "TableMort", definition = function(table, x, t = 1) {
    return(1 - pxt(table, x, t))
})

setMethod("Ext", "TableMort", definition = function(table, x, t = 1) {
    return(pxt(table, x, t) * v ^ t)
})

setMethod(f = "ex",
          signature = "TableMort",
          definition = function(table, x, continue = FALSE) {
    if (continue == FALSE) {
        # # version vectorisee
        return(sum(pxt(table, x, 0:200)))
    } else {
        stop("L'esperance continue n'est pas disponible pour les tables",
             "de mortalite en general, mais que pour les tables continue !")
    }
})

setMethod(f = "ax",
          signature = "TableMort",
          definition = function(table,
                                x,
                                temp = 200,
                                diff = 0,
                                m = 1,
                                due = TRUE,
                                continue = FALSE) {
    verb("methode ax de base called", 2)
    if (continue == FALSE && m < 1000) {
        # c'est c'est une anuity-due ( rente a terme echus), 
        # on calcule ce qu'il faut.  
        # c'est a terme a echoir, pas 'due, dot', ca va de 1 a N.  
        # version vectorisee
        decalage <- 1 - due
        # sum(v^(k)*pxt(x,k) avec k dans le domaine
        k <- (m * diff + decalage):(m * (diff + temp) - 1 + decalage)
        x <- matrix(x, nrow = length(table))
        plop <- x[1, ]
        for (i in 1:ncol(x)) {
            plop[i] <- sum(1 / m * Ext(table, x[, i], k / m))
        }
        return(plop)
    } else {
      stop("You cant calculate continuous anuities without knowing",
           "what kind of table you have (discrete or continuous ?)")
    }
})

setMethod(f = "Ax",
          signature = "TableMort",
          definition = function(table,
                                x,
                                temp = 200,
                                diff = 0,
                                m = 1,
                                continue = FALSE) {
    verb("methode Ax de base called", 2)
    if (continue == FALSE && m < 1000) {
        # c'est un capital, on calcule ce qu'il faut.  # version vectorisee
        k <- (m * diff):(m * (diff + temp) - 1)
        # on pourrais le vectoriser.
        x <- matrix(x, nrow = length(table))
        plop <- x[1, ]
        for (i in 1:ncol(x)) {
            plop[i] <- sum(sapply(k, 
              function(k) return(1 / m
                              * v ^ ( (k + 1) / m)
                              * pxt(table, x[, i], k / m)
                              * qxt(table, x + k / m, 1 / m))))
        }
        return(plop)
    } else {
        stop("You cant calculate continuous insurence without knowing",
             "what kind of table you have (discrete or continuous ?)")
    }
})

setMethod("is.cont", "TableMort", function(.Object) {
    if (methods::is(.Object, "TableMortContinue"))
        return(TRUE)
    return(FALSE)
})

setMethod("length", "TableMort", function(x) {
    return(1)
})

setMethod("isOk", "TableMort", function(table, x) {
    if (class(x) == "numeric")
        return(TRUE)
    stop("x doit etre numeric")
})

setMethod("summary", "TableMort", function(object, ...) {
    # On va ici essayer de faire un resume de la table de mortalite.
    if (length(object) > 1) {
        cat(" ~~~ ", if (is.cont(object)) {
            "Continuous"
        } else {
            "Discrete"
        }
        , "Life Table : Independent First death group ~~~\n")
        cat("     The underlying tables are the following one's : \n")
        kikou <- matrix(0)
        for (i in 1:length(object@sous_tables)) {
            kikou <- rbind(kikou,
                           as.matrix(paste("  ",
                            utils::capture.output(summary(object@sous_tables[[i]])))))
        }
        kikou <- kikou[-1, ]
        df <- (data.frame(kikou))
        colnames(df) <- c("")
        cat("          ")
        print(format(df, justify = "left"), row.names = F)
    } else {
        if (is.cont(object)) {
            cat(" ~~~ Continuous Table :",
                getName(object),
                "\n                        Life expectation @Birth : ",
                ex(object, 0, TRUE))
        } else {
            cat(" ~~~ Discrete Table :",
                getName(object),
                "\n                      Life expectation @Birth : ",
                ex(object, 0, FALSE),
                "\n                      Maximum Age : ",
                getAgeMax(object))
        }
    }
})
