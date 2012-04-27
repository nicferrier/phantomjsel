# package.el multi-file package install 

# These are the variables that are specific to the package
NAME=phantomjs
VERSION=0.0.11
DOC="Control phantomjs from Emacs"			
package_parts = phantomjs.el phantomjs-pkg.el ghostweb.js test.html config.js


# Everything beyond here should be generic
PACKAGE=$(NAME)-$(VERSION)
TARBALL=$(PACKAGE).tar 

all: tarball


# Install the tarball in a test package store
test: tarball
	emacs -Q --batch -l ./packagedir.el -- $(TARBALL)

# Install the tarball in the user's emacs
install: tarball
	emacs --batch --script ./build.el -- $(TARBALL)

clean:
	rm -rf .elpa
	rm -rf $(TARBALL)
	rm -rf $(PACKAGE) 
	rm -rf $(NAME)-pkg.el

tarball: $(TARBALL)

$(TARBALL): $(PACKAGE)
	tar cf $@ $<

$(PACKAGE): $(package_parts)
	mkdir $@
	cp $(package_parts) $@

$(NAME)-pkg.el:
	echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DOC)\")" > $@

# End
