shinylive::export(appdir = "myapp", destdir = "docs")
httpuv::runStaticServer("docs/", port = 8008)
getwd()

install.packages("MASS", type = "source")
install.packages("Matrix", type = "source")
install.packages("curl", type = "source")
