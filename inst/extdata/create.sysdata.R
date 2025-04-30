# saves relevant extdata files as sysdata.rda
attach("mn.den.rda")
attach("lxi.rda")
attach("lxi.mc.rda")
save(mn.n.line, lxi, lxi.mc, file="../../R/sysdata.rda")
tools::resaveRdaFiles("../../R/sysdata.rda")
