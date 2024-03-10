#1 Sampling without Replacement
?dhyper #Hypergeometric Distribution
#1a 
dhyper(1,30,1000-30,10)
dhyper(0,30,1000-30,10)
#1b 
?plot
xseq = c(0:10)
yseq = dhyper(xseq,30,10000-30,10)
plot(xseq,yseq)
#1c
yseq_s1000 = dhyper(xseq,30,970,10)
yseq_s100 = dhyper(xseq,30,100-30,10)
yseq_s50 = dhyper(xseq,30,50-30,10)
plot(xseq,yseq_s1000, col = "green")
points(xseq,yseq_s50, col = "blue")
points(xseq,yseq_s100, col = "red")


x=xseq
y1 <- dhyper(x,30,970,10)
y2 <- dhyper(x,30,70,10)
y3 <- dhyper(x,30,20,10)
plot(x,y1,"blue")
points(x,y2, "green")
points(x,y3,"red")
#2a 
dhyper(5,100,1000-100,20)
#2b
linesofcode = dhyper(5,100,1000-c(1:1000),20)
plot(linesofcode)
#2c


#3
plot(dbinom(c(0:40),40,0.1))
points(dbinom(c(0:40),40,0.5))
points(dbinom(c(0:40),40,0.7))
points(dbinom(c(0:40),40,0.9))
     