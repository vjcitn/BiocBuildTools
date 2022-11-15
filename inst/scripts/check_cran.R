x = available.packages()
allcran = rownames(x)
ii = installed.packages()
allcran_vers = x[,2]
intersect(rownames(ii), allcran) -> cin
cranvers_in = ii[cin,3]
oncran_vers = allcran_vers[names(cranvers_in)]
cranvers_in[which(cranvers_in != oncran_vers)]
