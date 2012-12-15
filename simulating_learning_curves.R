
# a is cost of first unit
# b is learning elasticity

a = 20
b1 = .3
b2 = .5
T = 10

t = c(1:T)
y1 = a*t^(-b1)
y2 = a*t^(-b2)

plot(y1~t, ylim = c(0, a), type = "l", col = 'red', main = "learning curves")
lines(y2~t, col = "blue")

text(6,13, "a = 20, b = .3")
text(6,9.5, "a = 20, b = .5")