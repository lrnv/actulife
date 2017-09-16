library(actulife)

# On peut avoir les Lx des tables :
Lx(th002d, 12.7)
Lx(th002v, 12, UDD = FALSE)
Lx(tf002v, 12.7, UDD = TRUE)
Lx(tf002d, 12.7, UDD = TRUE)

# Les escomptes viagés :
Ext(th002d, 12, 30)

# rentes :
ax(th002v, x = 3, temp = 2, diff = 4, m = 1, due = TRUE, continue = TRUE)
ax(th002d, x = 12, temp = 10, diff = 15, m = 1, due = TRUE, continue = TRUE)


# ex
ex(th002d, 0, FALSE)
ex(th002v, 18.3, FALSE)


# La fonction LX est vectorisé proprement
Lx(th002d, c(1, 2, 3))
# [1] 99511 99473 99446
