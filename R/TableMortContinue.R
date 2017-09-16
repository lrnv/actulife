#' @include Generics.R Verbosity.R TableMort.R
NULL



########################################################################################################## (Fille) TableMortContinue
#' @name TableMortContinue
#' @export
setClass(Class = "TableMortContinue",
         representation = representation(cdf = "function"),
         prototype = prototype(cdf = function(x) pexp(x)),
         contains = "TableMort",
         validity = function(object) {
    verb(" ~~~ TableMortContinue : verificateur ~~~\n", 2)
    if (object@cdf(rep(0, length(object))) != 0
      || object@cdf(rep(+Inf, length(object))) != 1) {
        stop ("[TableMortContinue : Validation] La fonction de distribution",
              "de la variable aleatoire de vie a la naissance doit valoir",
              "0 en 0 et 1 en +Inf")
    }
    if (class(object@name) != "character"
      || length(object@name) == 0
      || object@name == "") {
        stop ("[TableMortContinue : Validation] Le nom n'est pas valide")
    }
    return(TRUE)
})

tableMortContinue <- function(nom, fonction_de_repartition) {
    verb(" ~~~ TableMortContinue : Constructeur ~~~\n", 1)
    methods::new(Class = "TableMortContinue", cdf = fonction_de_repartition, name = nom)
}  # constructeur
##### Getteur et Setteur TableMortContinue

setMethod("getCdf", "TableMortContinue", function(object) {
    return(object@cdf)
})  # getCdf
##### Methodes et surcharges de methodes.

setMethod("show", "TableMortContinue", function(object) {
    cat(" ~~~ Table de mortalite : Continue ~~~\n")
    cat("     Nom de la table : ", getName(object), "\n")
})  # show

setMethod(f = "Lx",
          signature = "TableMortContinue",
          definition = function(table, x, UDD = TRUE) {
    # en continue, le calcul des lx est plus simple, et pas besoin de UDD...
    return(1e+05 * pxt(table, rep(0,length(table)), x))
})  # Lx

setMethod("pxt", "TableMortContinue", definition = function(table, x, t = 1) {
    # pour une variable aleatoire continue, tpx est juste egal a tp0 / xp0 :
    # Si la personne est deja morte en x quoi qu'il arrive, tpx n'a aucun sens.
    ifelse(getCdf(table)(x) == 1,
            stop("Vous essayez de calculer la proba de survie d'une personne deja morte"),
            return(sapply(t, function(p) {
      (1 - getCdf(table)(p + x)) / (1 - getCdf(table)(x))})))
})  # ptx

setMethod("muxt", "TableMortContinue", definition = function(table, x, t = 0) {
    ifelse(x + t <= 0,
           stop("Le taux de mortalite n'est pas definit en 0 !"),
           return(numDeriv::grad(function(p) -log(pxt(table, 0, p)), x + t)))
})  # muxt

setMethod(f = "ax",
          signature = "TableMortContinue",
          definition = function(table, x, temp = 200, diff = 0, m = 1,
                                due = TRUE, continue = FALSE) {
    verb("methode ax standard cont called", 2)
    if (continue == FALSE && m < 1000) {
        methods::callNextMethod()
    } else {
        # on doit ici calculer ax pour une variable continue
        return(stats::integrate(lower = diff, upper = diff + temp, f = function(t) Ext(table, x, t))$value)
    }
})  # ax

setMethod("Ax", "TableMortContinue", definition = function(table, x, temp = 200, diff = 0, m = 1, continue = FALSE) {
    verb("methode Ax standard cont called", 2)
    if (continue == FALSE && m < 1000) {
        methods::callNextMethod()
    } else {
        # On utilise ici UDD !!! Achtung !
        return(stats::integrate(lower = diff, upper = diff + temp, f = function(t) Ext(table, x, t) * muxt(table, x, t))$value)
    }
})  # Ax

setMethod("ex", "TableMortContinue", definition = function(table, x = 0, continue = FALSE) {
    if (continue == FALSE) {
        methods::callNextMethod()
    } else {
        return(stats::integrate(lower = 0, upper = +Inf, function(t) pxt(table, x, t))$value)
    }
})  # ex
