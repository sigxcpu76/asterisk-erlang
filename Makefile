REBAR:=./rebar
#PATH=/opt/local/bin:/opt/local/sbin:/usr/bin:/usr/sbin

.PHONY: all erl test clean doc

all: erl

erl:
	$(REBAR) -C rebar.config -j 1 get-deps compile
	@#@erl -make

erldebug:
	$(REBAR) -C rebar.debug.config -j 1

test: clean all
	@mkdir -p .eunit
	$(REBAR) -j 1 skip_deps=true eunit

clean:
	$(REBAR) clean
	@rm -rvf deps/*/ebin/* ebin/* doc .eunit

doc:
	$(REBAR) doc

 run: all
# 	#erl -pa ebin -boot start_sasl -s rmqclient_app start
# 	#erl +W w -pa ebin -pa deps/*/ebin -boot start_sasl -kernel error_logger=silent -sasl sasl_error_logger=false -sasl errlog_type=error -s start_app
# 	erl +K true +W w -pa ebin -pa deps/*/ebin -config cf -boot start_sasl -sasl sasl_error_logger=false -eval "application:start(acd_core), reloader:start()"
	erl +K true +W w -pa ebin -boot start_sasl -eval "reloader:start()"

# debug: clean erldebug
# 	erl +W w -pa ebin -pa deps/*/ebin -config cf -boot start_sasl -eval "application:start(acd_core), reloader:start()"
