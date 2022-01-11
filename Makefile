PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGDATE = `date +%Y-%m-%d`
PKGYEAR = `date +%Y`

all: check

build: examples-as-tests
	mkdir -p builds
	@rm -rf /tmp/$(PKGNAME)
	cp -R ./ /tmp/$(PKGNAME)
	# set tags
	{ \
		cd /tmp/$(PKGNAME) ;\
		for f in `ls R/*.R man/*.Rd DESCRIPTION LICENSE`; do \
			echo "processing $$f" ;\
			sed \
				-e "s/\$$version\$$\?/$(PKGVERS)/g" \
				-e "s/\$$date\$$\?/$(PKGDATE)/g" \
				-e "s/\$$year\$$\?/$(PKGYEAR)/g" \
				< $$f > $$$$; \
			mv $$$$ $$f; \
		done; \
	}
	cd builds; R CMD build /tmp/$(PKGNAME)
	@rm -rf /tmp/$(PKGNAME)


# copy examples from man/ files into tests/testthat/test-examples.R
examples-as-tests:
	@Rscript -e "\
    sink('tests/testthat/test-examples.R') ;\
    cat('library(ggplot2)\n\n') ;\
    invisible(lapply(list.files('man', pattern = '\\\.Rd$$', full.names = TRUE), function(f) { ;\
      x <- unlist(Filter(function(x) attr(x, 'Rd_tag') == '\\\examples', tools::parse_Rd(f))) ;\
      if(length(x)) cat('\\U0023 example(s) from: ', f, '\n', x, '\n', sep = '') ;\
      NULL ;\
    })) ;\
    sink() ;\
    "

check: build
	R CMD check builds/$(PKGNAME)_$(PKGVERS).tar.gz

install:
	R CMD INSTALL --library=$(R_LIBS_USER) builds/$(PKGNAME)_$(PKGVERS).tar.gz

clean:
	@rm -rf $(PKGNAME).Rcheck
