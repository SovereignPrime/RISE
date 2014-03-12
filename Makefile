
NITROGEN_VERSION=2.2.2

help:
	@echo 
	@echo "Usage: "
	@echo "       $(MAKE) {compile|clean}"        
	@echo
	@echo "       $(MAKE) {linux|win|mac}"
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
	
# PLATFORM-SPECIFIC RISE

linux:
	@($(MAKE) rel PLATFORM=linux)
	@(git clone git://github.com/SovereignPrime/RISE-frontend.git rel/frontend)

mac:
	@($(MAKE) rel PLATFORM=mac)
	@(git clone git://github.com/SovereignPrime/RISE-macosx-frontend.git rel/frontend)

win:
	@($(MAKE) rel_win PLATFORM=cowboy)
	@(git clone git://github.com/SovereignPrime/RISE-frontend.git rel/frontend)


# PLATFORM-AGNOSTIC

## TODO: simplify further by adding a $(MODE) argument to be used in place of rel_inner_slim and rel_inner_full
rel: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_$(PLATFORM).config)
	@($(MAKE) rel_inner_full PLATFORM=$(PLATFORM))
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/rise', configured to run on $(PLATFORM).

rel_win: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_cowboy.config reltool_win.config)
	@($(MAKE) rel_inner_win PLATFORM=$(PLATFORM))
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on $(PLATFORM).

# MASS PACKAGING - Produce packages for all servers

clean_docs:
	@(cd rel/nitrogen; rm -fr doc)

copy_docs: clean_docs
	@(echo "Copying Documentation to the release")
	@(cd rel/nitrogen; cp -r lib/nitrogen_core/doc .; cd doc; rm *.pl *.html)

link_docs: clean_docs
	@(echo "Linking Documentation in the release")
	@(cd rel/nitrogen; ln -s lib/nitrogen_core/doc doc)

# TRAVIS-CI STUFF

ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: $(ERLANG_VERSION)

R14B02: rel_inets rel_yaws rel_mochiweb rel_webmachine
R14B03: R14B02
R15B: R14B02 rel_cowboy
R15B01: R15B
R15B02: R15B slim_cowboy slim_inets slim_yaws slim_mochiweb slim_webmachine
R15B03: R15B02
R16B: R15B02
R16B01: R16B
R16B02: R16B

# SHARED

clean_release:
	@(rm -rf rel/rise)
	@(rm -rf rel/frontend)
	@(rm -rf rel/reltool.config)

generate:
	@(cd rel; ./rebar generate)

erl_interface:
	@(cd rel; escript copy_erl_interface.escript)

rel_inner:
	@(cd rel; cp overlay/rebar.config.src rise/rebar.config)
	@(cd rel/rise; git clone git://github.com/SovereignPrime/RISE-nitrogen-site.git ./site)
	@(cd rel/rise; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/rise/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/rise/BuildInfo.txt
	@uname -v >> rel/rise/BuildInfo.txt
	@rm -rf rel/reltool.config	

rel_inner_slim:
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) generate rel_inner PLATFORM=$(PLATFORM))

rel_inner_full: generate erl_interface rel_inner


rel_inner_win: generate erl_interface
	@(cd rel/nitrogen; cp releases/${NITROGEN_VERSION}/start_clean.boot bin/)
	@(cd rel; ./merge_platform_dependencies.escript overlay/rebar.config.src overlay/$(PLATFORM).deps nitrogen/rebar.config)
	@(cd rel/rise; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@(cd rel/rise; ./make_start_cmd.sh)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@rm -rf rel/reltool.config rel/nitrogen/make_start_cmd.sh rel/nitrogen/start.cmd.src

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
	$(foreach app,$(wildcard deps/*), rm -rf rel/nitrogen/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/nitrogen/lib;)


