#' @include Generics.R Verbosity.R TableMort.R TableMortDiscrete.R TableMortContinue.R TableMortGroupePDI.R TableMortGroupePDIDiscrete.R TableMortGroupePDIContinue.R
NULL



################################################ (Union virtuelle) Tables
setClassUnion("Tables", members = "TableMort")
############################# Classe Assure

#' @name Assure
#' @export
setClass(Class = "Assure",
         representation = representation(IsMale = "logical",
                                         Name = "character",
                                         Age = "numeric",
                                         Tbl = "Tables"),
         validity = function(object) {
    verb(" ~~~ Assure : verificateur ~~~\n", 2)
    if (!(length(object@Tbl) == length(object@Age))) {
        return(FALSE)
    }
    if (!prod(object@Age > 0)) {
        stop("[Assure : Validation] Votre assure a un age negatif !")
    }
    return(TRUE)
})

# setMethod(f = "initialize",
#           signature = "Assure",
#           definition = function(.Object, IsMale, Name, Age, Mortality) {
#     verb(" ~~~ Assure : Initiateur ~~~\n", 2)
#     .Object@Tbl <- Mortality
#     .Object@IsMale <- IsMale
#     .Object@Name <- Name
#     .Object@Age <- Age
#     methods::validObject(.Object)
#     return(methods::as(.Object, "Assure"))
# })

assure <- function(Nom, Sexe, Age, Table_de_Mortalite) {
    verb(" ~~~ Assure : Constructeur ~~~\n", 1)
    if (!prod(ifelse( (Sexe == "F") + (Sexe == "H"), TRUE, FALSE))) {
        stop("[Assure : Constructeur] La variable Sexe doit etre 'H' ou 'F' ")
    }
    methods::new(Class = "Assure", Sexe == "H", Name = Nom, Age = Age, Tbl = Table_de_Mortalite)
}

##### Getteur et Setteur Assure
setMethod(f = "setAge", "Assure",
          definition = function(object, Age) {
    if (prod(object@Age < 0)) {
        stop("[Assure : Validation] Votre assure a un age negatif !")
    } else {
        object@Age <- Age
    }
})

setMethod(f = "getSexe",
          signature = "Assure",
          definition = function(object) {
    return(object@Sexe)
})

setMethod(f = "setSexe",
          signature = "Assure",
          definition = function(object, Sexe) {
    if (Sexe == "F" || Sexe == "H") {
        object@IsMale <- (Sexe == "H")
    } else {
        stop("[Assure : setSexe] La variable Sexe doit etre 'H' ou 'F' ")
    }
})

setMethod("getName",
          "Assure",
          definition = function(object) {
    return(object@Name)
})

setMethod("getAge", "Assure", definition = function(object) {
    return(object@Age)
})

setMethod("length", "Assure", definition = function(x) {
    return(length(getAge(x)))
})

##### Methodes et surcharges de methodes.
setMethod("print", "Assure", function(x, ...) {
    cat(x@Name, " / Male = ", x@IsMale, " / ", x@Age, "ans.")
})

setMethod("show", "Assure", function(object) {
    sexe <- paste(ifelse(object@IsMale, "Homme", "Femme"), sep = " & ")
    cat(" ~~~ Assure : ", object@Name, " ~~~\n")
    cat("     Sexe   : ", sexe, "\n")
    cat("     Age    : ", object@Age, "ans\n")
    cat("     Table  : ", getName(object@Tbl))
})

setMethod("c", signature(x = "Assure"), function(x, ...) {
    assures <- list(x, ...)
    if (length(assures) == 1) {
        return(assures)
    }
    # commencons par check que c'est bien des assures :
    Nom <- ""
    Age <- vector()
    IsMale <- numeric()
    Tables <- c()
    for (ass in assures) {
        if (!methods::is(ass, "Assure")) {
        stop("Vous essayez de merger de objects qui ne sont pas des assures !")
        }
        Nom <- paste(Nom, "&", ass@Name)
        Age <- c(Age, getAge(ass))
        IsMale <- c(IsMale, ass@IsMale)
    }
    Nom <- substring(Nom, 4)
    for (i in 1:length(assures)) {
        if (length(assures[[i]]@Tbl) == 1) {
            Tables <- c(Tables, assures[[i]]@Tbl)
        } else {
            for (j in 1:length(assures[[i]]@Tbl)) {
                Tables <- c(Tables, assures[[i]]@Tbl@sous_tables[[j]])
            }
        }
    }
    table <- tableMortGroupePDI(Tables, nom = paste("Loi de :", Nom))
    return(assure(Nom, ifelse(IsMale, "H", "F"), Age, table))
})
