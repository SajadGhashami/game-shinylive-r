shinylive::export(appdir = "myapp", destdir = "docs")
httpuv::runStaticServer("docs/", port = 8008)
getwd()
