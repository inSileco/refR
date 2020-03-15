context("Test reference extraction")
####
file.name <- system.file('Bib', 'RJC.bib', package='RefManageR')
bib <- RefManageR::ReadBib(file.name)
tfile <- tempfile(fileext = '.md')
tfile2 <- tempfile(fileext = '.tex')
####
txt <- 'First \\cite{kim1995estimating}, second \\Citep{fu2006statistical}'
txtmd <- 'First @Pimm2000, second @May1972, third [@May1972]'
cat(txtmd, sep='\n', file=tfile)
cat(txt, sep='\n', file=tfile2)
####
out <- extractBib(bib, txt)
out2 <- extractBib(bib, con = tfile2)
res <- findRef(text = 'First \\cite{Pimm2000}, second \\Citep{May1972}')
res2 <- findRef(text = txtmd, markdown = TRUE)
res3 <- findRef(con = tfile, markdown = TRUE)

test_that("findRef", {
  expect_error(findRef(), "Either 'text' or 'con' must be specified.", fixed = TRUE)
  expect_true(all(res$key == c("May1972", "Pimm2000")))
  expect_true(all(res2$key == c("May1972", "Pimm2000")))
  expect_true(all(res$freq == c(1,1)))
  expect_true(all(res2$freq == c(2,1)))
  expect_true(all(res2==res3))
})


ids <- which(bib$key %in% c("fu2006statistical", "kim1995estimating"))

test_that("extractBib", {
  expect_error(extractBib("tmp"), 'any(class(bib) %in% c("BibEntry", "bibentry")) is not TRUE', fixed = TRUE)
  expect_error(extractBib(bib), "Either 'text' or 'con' must be specified.")
  expect_warning(extractBib(bib, "no ref"))
  expect_identical(out$keys, c("fu2006statistical", "kim1995estimating"))
  expect_identical(out$keys, out2$keys)
  expect_identical(out$refs, bib[ids])
  expect_identical(out$refs, out2$refs)
})
