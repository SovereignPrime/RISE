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


## Produce a list of contributors from the main repo and the dependent repos
thanks: 
	@(cd support/list_thanks; \
	rm -fr simple_bridge nprocreg nitrogen_core NitrogenProject.com; \
	git clone git://github.com/nitrogen/simple_bridge; \
	git clone git://github.com/nitrogen/nprocreg; \
	git clone git://github.com/nitrogen/nitrogen_core; \
	git clone git://github.com/nitrogen/NitrogenProject.com; \
	perl list_thanks.pl >> ../../thanks.txt; \
	rm -fr simple_bridge nprocreg nitrogen_core NitrogenProject.com; \
	echo "Thanks file generated in thanks.txt - please review")
	
deb: 
	@(echo "Creating DEB package for rise-{{rel_vsn}}")
	# @(cd rel/Release; sudo chown -R root:root rise-$(RISE_VERSION); chmod 644 rise-$(RISE_VERSION)/DEBIAN/conffiles)
	dpkg-deb -z8 -Zgzip --build rise-{{rel_vsn}}

mac:
	@($(MAKE) rel PLATFORM=mac)
	@($(MAKE) mac_frontend PLATFORM=mac)
	@($(MAKE) dmg PLATFORM=mac)

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

mac_frontend:
	@(rm -rf rel/frontend)
	@(git clone git://github.com/SovereignPrime/RISE-macosx-frontend.git rel/frontend)
	@(cd rel/frontend; xcodebuild && mkdir ../Release)
	@(cd rel; cp -r frontend/build/Release/RISE.app Release/RISE.app && cp -r rise Release/RISE.app/Contents/Backend && ln -s /Applications ./Release/Applications) 

dmg:
	@(cd rel; hdiutil create ./Release/RISE_${RISE_VERSION}.dmg -volname RISE -srcdir ./Release)

setup:
	@(rm -rf rel/Release)
	@(iscc packages/win/rise.iss)
