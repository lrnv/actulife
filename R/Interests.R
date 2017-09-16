###### Main variables and functions about interests that life insurence uses :

# Donnee de i :
i <- 0.03
v <- 1 / (1 + i)
d <- 1 - v
delta <- -log(v)

# fonction financieres
im <- function(m) {
    if (m == +Inf) {
        return(delta)
    } else {
        return (m * ( (1 + i) ^ (1 / m) - 1))
    }
}

dm <- function(m = 1) {
    if (m == +Inf) {
        return (delta)
    } else {
        return (1 - v ^ (1 / m))
    }
}

alpha <- function(m) {
    return (i * d / (im(m) * dm(m)))
}

beta <- function(m) {
    return ((i - im(m))/(im(m) * dm(m)))
}

# Operateur difference ( pour shuette-nesbitt) :
DELTA <- function(k, f, x) {
    # verify that k and x are roundnumbers :
    if (k != round(k) || x != round(x)) {
        stop ("k et x doivent etre des entiers.")
    }
    if (k == 0) {
        return (f(x))  # operateur identite
    } else {
        newf <- function(x) {
            f(x + 1) - f(x)
        }
        return (DELTA(k - 1, newf, x))
    }
}
