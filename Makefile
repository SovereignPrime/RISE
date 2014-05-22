RISE_VERSION = $(shell git describe --tags --abbrev=0)

help:
	@echo 
	@echo "Usage: "
	@echo "       $(MAKE) {compile|clean}"        
	@echo
	@echo "       $(MAKE) {linux|deb|win|mac}"
	@echo
	@echo 

all: get-deps compile

distribute-rebar:
	@(cp rebar rel/rebar; cp rebar rel/overlay/common;)

get-deps: distribute-rebar
	./rebar get-deps

update-deps:
	./rebar update-deps

compile: get-deps
	./rebar compile

clean:
	./rebar clean

install-helper-script:
	@(cd support/helper_script;./install.sh)

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
	
version:
	@(echo "Building RISE-$(RISE_VERSION)")
	@(sed 's/".[0-9]*\.[0-9]*\.[0-9]*"/"$(RISE_VERSION)"/' rel/reltool_base.config > rel/reltool_tmp.config)


# PLATFORM-SPECIFIC RISE

linux: version
	@($(MAKE) rel PLATFORM=linux)
	@(git clone git://github.com/SovereignPrime/RISE-frontend.git rel/frontend)
	@(cd rel/frontend; qmake sp-rise.pro -config release && make)
	@(cd rel; cp frontend/rise rise/bin/rise)
	

deb: linux
	@(echo "Creating DEB package for rise-$(RISE_VERSION)")
	@(mkdir rel/Release)
	@(mkdir -p rel/Release/rise-$(RISE_VERSION)/opt)
	@(mkdir -p rel/Release/rise-$(RISE_VERSION)/DEBIAN)
	@(mkdir -p rel/Release/rise-$(RISE_VERSION)/etc/profile.d)
	@(mkdir -p rel/Release/rise-$(RISE_VERSION)/etc/init)
	@(mkdir -p rel/Release/rise-$(RISE_VERSION)/usr/bin)
	@(cd rel; mv rise Release/rise-$(RISE_VERSION)/opt)
	@(cd rel/Release/rise-$(RISE_VERSION); cp opt/rise/etc/rise.sh etc/profile.d)
	@(cd rel/Release/rise-$(RISE_VERSION); cp opt/rise/bin/rise usr/bin)
	@(cp -r packages/debian/* rel/Release/rise-$(RISE_VERSION))
	@(cd rel/Release; dpkg-deb -z8 -Zgzip --build rise-$(RISE_VERSION))

mac: version
	@($(MAKE) rel PLATFORM=mac)
	@(git clone git://github.com/SovereignPrime/RISE-macosx-frontend.git rel/frontend)
	@(cd rel/frontend; xcodebuild && mkdir ../Release)
	@(cd rel; cp -r frontend/build/Release/RISE.app Release/RISE.app && cp -r rise Release/RISE.app/Contents/Backend && ln -s /Applications ./Release/Applications) 
	@(cd rel; hdiutil create ./Release/RISE_${RISE_VERSION}.dmg -volname RISE -srcdir ./Release)

win: version
	@(CC=gcc && $(MAKE) rel_win PLATFORM=win)
	@(git clone git://github.com/SovereignPrime/RISE-frontend.git rel/frontend)
	@(cd rel/frontend; qmake sp-rise.pro -config release && make)
	@(cd rel; cp frontend/release/rise.exe rise/bin/rise.exe)
	@(cd rel/rise; erts-6.0/bin/escript.exe merge-configs.escript ./etc)
	@(iscc packages/win/rise.iss)


# PLATFORM-AGNOSTIC

## TODO: simplify further by adding a $(MODE) argument to be used in place of rel_inner_slim and rel_inner_full
rel: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_tmp.config reltool_$(PLATFORM).config)
	@($(MAKE) rel_inner_full PLATFORM=$(PLATFORM))
	@echo Generated a self-contained Rise project
	@echo in 'rel/rise', configured to run on $(PLATFORM).

rel_win: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_tmp.config reltool_win.config)
	@($(MAKE) rel_inner_win PLATFORM=$(PLATFORM))
	@echo Generated a self-contained Rise project
	@echo in 'rel/rise', configured to run on $(PLATFORM).

# MASS PACKAGING - Produce packages for all servers

clean_docs:
	@(cd rel/rise; rm -fr doc)

copy_docs: clean_docs
	@(echo "Copying Documentation to the release")
	@(cd rel/rise; cp -r lib/nitrogen_core/doc .; cd doc; rm *.pl *.html)

link_docs: clean_docs
	@(echo "Linking Documentation in the release")
	@(cd rel/rise; ln -s lib/nitrogen_core/doc doc)

# TRAVIS-CI STUFF

ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: $(ERLANG_VERSION)

R16B: linux mac win
R16B01: R16B
R16B02: R16B

# SHARED

clean_release:
	@(rm -rf rel/rise)
	@(rm -rf rel/frontend)
	@(rm -rf rel/reltool.config)
	@(rm -rf rel/Release)

generate:
	@(cd rel; ./rebar generate)

erl_interface:
	@(cd rel; escript copy_erl_interface.escript)

rel_inner:
	@(cd rel; cp overlay/rebar.config.src rise/rebar.config)
	@(cd rel/rise; git clone git://github.com/SovereignPrime/RISE-nitrogen-site.git ./site)
	@(cd rel/rise; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@printf "Rise Version:\n${RISE_VERSION}\n\n" > rel/rise/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/rise/BuildInfo.txt
	@uname -v >> rel/rise/BuildInfo.txt
	@rm -rf rel/reltool.config	

rel_inner_full: generate erl_interface rel_inner


rel_inner_win: generate erl_interface
	@(cd rel; cp overlay/rebar.config.src rise/rebar.config)
	@(cd rel/rise; git clone git://github.com/SovereignPrime/RISE-nitrogen-site.git ./site)
	@(cd rel/rise; cp releases/${RISE_VERSION}/start_clean.boot bin/)
	@(cd rel/rise; $(MAKE); $(MAKE) cookie)
	@(cd rel/rise; ./make_start_cmd.sh)
	@printf "Rise Version:\n${RISE_VERSION}\n\n" > rel/rise/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/rise/BuildInfo.txt
	@uname -v >> rel/rise/BuildInfo.txt
	@rm -rf rel/reltool.config rel/rise/make_start_cmd.sh rel/rise/start.cmd.src

rel_copy_quickstart:
	mkdir -p deps
	(rm -fr deps/NitrogenProject.com)
	(cd deps; git clone git://github.com/nitrogen/NitrogenProject.com.git)
	cp -R deps/NitrogenProject.com/src/* rel/nitrogen/site/src
	cp -R deps/NitrogenProject.com/static/* rel/nitrogen/site/static
	cp -R deps/NitrogenProject.com/templates/* rel/nitrogen/site/templates
	rm -rf rel/nitrogen/site/src/nitrogen_website.app.src
	(cd rel/nitrogen; ln -s site/static static)
	(cd rel/nitrogen; ln -s site/templates templates)

rellink:  
	$(foreach app,$(wildcard deps/*), rm -rf rel/rise/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/rise/lib;)

