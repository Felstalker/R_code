# TODO: Add comment
#   to alter: Preferences->StatET->Code Generation
# Author: OGOREKB
###############################################################################
setInternet2(TRUE)


#remember, I'm calculating the magnitude of the imaginary number!

v <- c(0:49)/100
f.r <- (1/3 + (2/3)*cos(2*pi*v))^2
plot(f.r ~ v)


f.r2 <- (.5 - .5*cos(2*pi*v))^2 + (.5*sin(2*pi*v))^2
plot(f.r2 ~ v)

head(co2)
plot(stl(log(co2), s.window="per", t.window=1000))


