PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

doc:
	${RSCRIPT} -e 'devtools::document()'

build:
	R CMD build .

install: doc build
	R CMD INSTALL . && rm *.tar.gz

installvign:
	${RSCRIPT} -e 'devtools::install(build_vignettes=TRUE)'

readme: README.Rmd
	${RSCRIPT} -e "knitr::knit('$<')"

eg:
	${RSCRIPT} -e "devtools::run_examples(run_dontrun = TRUE)"

checkwin:
	${RSCRIPT} -e "devtools::check_win_devel(quiet = TRUE); devtools::check_win_release(quiet = TRUE)"

test:
	${RSCRIPT} -e "devtools::test()"

vign:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('rtaxonworks.Rmd.og', output = 'rtaxonworks.Rmd')";\
	cd ..

# update api spec and params in functions
spec_update:
	${RSCRIPT} -e "source('inst/scripts/update_spec.R')"

spec_pkg_update:
	${RSCRIPT} -e "source('inst/scripts/update_package.R')"

# No real targets!
.PHONY: all check document install
