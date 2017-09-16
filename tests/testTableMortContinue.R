library(actulife)

# On peut avoir les Lx des tables :
Lx(expolaw, 12.7)
Lx(expolaw2, 12.7)

# Les escomptes viagés :
Ext(expolaw, 12, 30)

# rentes :
ax(expolaw2, x = 0, due = TRUE, continue = TRUE)
ax(expolaw, x = 37, temp = 2, diff = 4, m = 1, due = TRUE, continue = TRUE)

# muxt
muxt(expolaw2, 15)
muxt(expolaw, 12)
# muxt(expolaw2,0) should fail

# ex
ex(expolaw2, 0, TRUE)
ex(expolaw2, 0)


getCdf(expolaw)(c(1, 2, 3))
# [1] 0.009950166 0.019801327 0.029554466
