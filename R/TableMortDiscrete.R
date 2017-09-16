#' @include Generics.R Verbosity.R TableMort.R
NULL




########################################################################################################## (Fille) TableMortDiscrete
#' @name TableMortDiscrete
#' @export
setClass(Class = "TableMortDiscrete", representation = representation(table = "data.frame"), prototype = prototype(table = data.frame()), contains = "TableMort", validity = function(object) {
    verb(" ~~~ TableMortDiscrete : verificateur ~~~\n", 2)
    if ( (length(object@table) != 2) || (names(object@table) != c("Age", "Lx"))) {
        stop("[TableMortDiscrete : Validation] le data.frame pour construire une table discrete doit avoir 2 collones : une apellee 'Age', et une apellee 'Lx'")
    }
    if (class(object@name) != "character" || length(object@name) == 0 || object@name == "") {
        stop("[TableMortDiscrete : Validation] Le nom n'est pas valide")
    }
    return(TRUE)
})

tableMortDiscrete <- function(nom, table) {
    verb(" ~~~ TableMortDiscrete : Constructeur ~~~\n", 1)
    methods::new(Class = "TableMortDiscrete", table = table, name = nom)
}  # constructeur
##### Getteur et Setteur TableMortDiscrete

setMethod("getCdf", "TableMortDiscrete", function(object, UDD = TRUE) {
    # pour des questions de compatibilite avec les tables continues.  sera utile pour les groupes.
    kikou <- object@table$Age
    kikou[1] <- 0
    for (i in 2:length(object@table$Age)) {
        kikou[i] <- object@table$Lx[i - 1] - object@table$Lx[i]
    }
    if (UDD) {
        # si UDD est allowed, on sort une interpolation lineaire.
        ecdf2 <- function(x) {
            x <- sort(x)
            n <- length(x)
            if (n < 1)
                stop("'x' must have 1 or more non-missing values")
            vals <- unique(x)
            rval <- stats::approxfun(vals, cumsum(tabulate(match(x, vals))) / n, method = "linear", yleft = 0, yright = 1, f = 0, ties = "ordered")
            class(rval) <- c("ecdf", class(rval))
            assign("nobs", n, envir = environment(rval))
            attr(rval, "call") <- sys.call()
            rval
        }
        return(Ecdf = ecdf2(kikou))
    } else {
        return(Ecdf = stats::ecdf(kikou))
    }
})

setMethod("getTable", "TableMortDiscrete", function(object) {
    return(object@table)
})  # getTable

setMethod("getAgeMax", "TableMortDiscrete", function(object) {
    return(max(object@table$Age))
})  # getAgeMax
##### Methodes et surcharges de methodes.

setMethod("show", "TableMortDiscrete", function(object) {
    cat(" ~~~ Table de mortalite : Discrete ~~~\n")
    cat("     Nom de la table : ", getName(object), "\n")
    cat("     Age maximum = ", getAgeMax(object))
})  # show

setMethod("Lx", "TableMortDiscrete", definition = function(table, x, UDD = TRUE) {
    udd_interpolation <- function(x) {
        # la formule d'interpolation des LX sous udd est la suivante : l(x+s) = (1-s) * l(x) + s*l(x+1)
        return( (1 - x + floor(x)) * getTable(table)$Lx[floor(x) + 1] + (x - floor(x)) * getTable(table)$Lx[x + 2])
    }
    # Version vectorized du truc : Lx doit tourner en vectoriel comme il faut !
    kikou <- ifelse(x > length(getTable(table)$Lx) - 1, 0, ifelse(x != floor(x), if (UDD) {
        # Sinon, si c'est pas un entier, on fait UDD si allowed, ERROR sinon
        udd_interpolation(x)
    } else {
        stop("Vous avez demander un age non-entier sur une table discrete,\n Vous devriez prendre une assomption de continuitee (UDD, CFM...).")
    }
    , getTable(table)$Lx[x + 1]  # Si c'est pas trop vieux, que c'est entier et tout, on retourne ce qu'il faut.
))
    return(kikou)
})  # Lx

setMethod("pxt", "TableMortDiscrete", definition = function(table, x, t = 1) {
    return (sapply(t, function(p) ifelse(Lx(table, x + p - p) == 0, 1, Lx(table, x + p)/Lx(table, x))))
})  # ptx

setMethod("ax", "TableMortDiscrete", definition = function(table, x, temp = 200, diff = 0, m = 1, due = TRUE, continue = FALSE) {
    verb("methode ax standard discret called", 2)
    if (continue == FALSE && m < 1000) {
        methods::callNextMethod()
    } else {
        # on doit ici calculer ax continue pour une variable discrete, sous UDD Attention on utilise ici UDD !!!!
        return(alpha(+Inf) * ax(table, x, temp, diff, m = 1, due = TRUE, continue = FALSE) + beta(+Inf) * Ext(table, x, diff) * (1 - Ext(table, x + diff, temp)))
    }
})  # ax

setMethod("Ax", "TableMortDiscrete", definition = function(table, x, temp = 200, diff = 0, m = 1, continue = FALSE) {
    verb("methode Ax tablediscrete called", 2)
    if (continue == FALSE && m < 1000) {
        methods::callNextMethod()
    } else {
        # On utilise ici UDD !!! Achtung !
        return( (i/delta) * (Ax(table, x, temp, diff, m = 1, continue = FALSE)))
    }
})  # Ax
