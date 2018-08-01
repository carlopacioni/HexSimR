library(HexSimR)
context("compress log files")
tmp <- tempdir()
dir.create(file.path(tmp, "test"))
cat(file=file.path(tmp, "test", "foo.log"), "Hello world!")

compress.logs(path.results=tmp, scenarios="test", delete.log=FALSE)

expect_true(file.exists(file.path(tmp, "test", "foo.log")))
expect_true(file.exists(file.path(tmp, "test", "foo.gz")))

expect_error(
  compress.logs(path.results=tmp, scenarios="test", delete.log=FALSE, overwrite.gz=FALSE)
  )
 
compress.logs(path.results=tmp, scenarios="test", delete.log=TRUE, overwrite.gz=TRUE)

expect_false(file.exists(file.path(tmp, "test", "foo.log")))

unlink(tmp, recursive=TRUE)
