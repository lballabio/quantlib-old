
SITE = shell.sf.net://home/groups/q/qu/quantlib/htdocs

TIDYOPTS = -q -m

INCLUDES = qlmeta.inc qlheader.inc qlfooter.inc
FILES    = index.shtml books.shtml cvs.shtml download.shtml \
           developerFAQ.shtml extensions.shtml license.shtml \
           lists.shtml mailinglists.shtml marketconventions.shtml \
           quep.shtml style.shtml

.PHONY: all check install

all:
	@echo "Available targets:"
	@echo "    make check                     - run files through tidy"
	@echo "    SFUSER=username make install   - copy files to web server"

check:
	tidy ${TIDYOPTS} ${FILES}

install:
	scp ${INCLUDES} ${SFUSER}@${SITE}
	scp ${FILES} ${SFUSER}@${SITE}
	scp images/*.gif images/*.jpg images/*.png ${SFUSER}@${SITE}/images
	scp styles/*.css ${SFUSER}@${SITE}/styles
