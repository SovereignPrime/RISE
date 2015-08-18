RISE_VERSION = $(shell git describe --tags --abbrev=0)

help:
	@echo 
	@echo "Usage: "
	@echo "       $(MAKE) {compile|clean}"        
	@echo
	@echo "       $(MAKE) {lin|deb|win|mac}"
	@echo
	@echo 

all: 
	./rebar3 do clean -a, update, compile, release

compile:
	./rebar3 compile

clean:
	./rebar3 clean -a

distclean: 
	@(rm -rf _build)
	@(rm -rf frontend)

deb: 
	@(echo "Creating DEB package for rise-{{rel_vsn}}")
	dpkg-deb -z8 -Zgzip --build rise-{{rel_vsn}}

win:
	@(CC=gcc && $(MAKE) rel_win PLATFORM=win)
	@($(MAKE) frontend PLATFORM=win)
	@(cd rel; cp frontend/release/rise.exe rise/bin/rise.exe)
	@(cd rel/rise; erts-6.0/bin/escript.exe merge-configs.escript ./etc)
	@($(MAKE) setup)

# SHARED

clean_release:
	@(rm -rf rel/rise)
	@(rm -rf rel/frontend)
	@(rm -rf rel/reltool.config)
	@(rm -rf rel/Release)

lin:
	@(rm -rf frontend)
	@(git clone git://github.com/SovereignPrime/RISE-frontend.git frontend)
	@(cd frontend; qmake sp-rise.pro -config release && make)

mac:
	@(rm -rf frontend)
	@(git clone git://github.com/SovereignPrime/RISE-macosx-frontend.git frontend)
	@(cd frontend; xcodebuild)

dmg:
	hdiutil create ./Release/RISE_{{rel_vsn}}.dmg -volname RISE-{{rel_vsn}} -srcdir ./Release

setup:
	@(rm -rf rel/Release)
	@(iscc packages/win/rise.iss)
