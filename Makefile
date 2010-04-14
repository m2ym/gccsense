VERSION=`etc/version`
PACKAGE=gccsense-$(VERSION)

thanks:
	@echo 'Thanks for downloading!'
	@echo
	@echo 'README:'
	@cat README

clean:
	rm -f doc/*.html
	rm -rf ${PACKAGE}
	rm -f ${PACKAGE}.tar.bz2

package: clean
	mkdir ${PACKAGE}
	cp -rp Makefile README TODO GPL.txt bin doc etc ${PACKAGE}

tar.bz2: package
	tar cjf ${PACKAGE}.tar.bz2 ${PACKAGE}
