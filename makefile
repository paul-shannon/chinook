all:  docs install

docs:
	R -e "devtools::document()"
build:
	(cd ..; R CMD build --no-build-vignettes Chinook)

install:
	(cd ..; R CMD INSTALL Chinook)

check:
	(cd ..; R CMD check `ls -t Chinook) | head -1`)

test:
	for x in inst/unitTests/test_*.R; do echo $$x; R -f $$x; done

