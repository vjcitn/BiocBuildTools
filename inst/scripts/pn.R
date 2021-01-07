tag="demo16"
gitfolder = paste0("/tmp/", tag, "_srcs")
sqlitetarget = paste0("/tmp/", tag, ".sqlite")
pnettarget = paste0("/tmp/", tag, "_pnet.rds")
alldirs = dir(gitfolder, full=TRUE)
srcs = alldirs

make_pnet_object(srcs, target=pnettarget)
