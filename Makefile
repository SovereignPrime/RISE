RISE_VERSION = $(shell git describe --tags --abbrev=0)

help:
	@echo 
	@echo "Usage: "
	@echo "       $(MAKE) {compile|clean}"        
	@echo
	@echo "       $(MAKE) {lin|deb|win|mac}"
	@echo
	@echo 

all: distclean
	./rebar3 do update, compile, release

compile:
	./rebar3 compile

clean:
	./rebar3 clean -a

distclean: 
	@(rm -rf _build)
	@(rm -rf frontend)


qt:
	@(rm -rf frontend)
	@(git clone git://github.com/SovereignPrime/RISE-frontend.git frontend)
	@(cd frontend; qmake sp-rise.pro -config release && make)

xcode:
	@(rm -rf frontend)
	@(git clone git://github.com/SovereignPrime/RISE-macosx-frontend.git frontend)
	@(cd frontend; xcodebuild)

dmg:
	@(hdiutil create ./Release/RISE_{{rel_vsn}}.dmg -volname RISE-{{rel_vsn}} -srcdir ./Release)

setup:
	@(iscc meta/win/rise.iss)

deb: 
	@(echo "Creating DEB package for rise-{{rel_vsn}}")
	@(dpkg-deb -z8 -Zgzip --build rise-{{rel_vsn}})
