REBAR = $(shell which rebar || echo './rebar')
DEPSOLVER_PLT = ./.dialyzer_plt

.PHONY: compile dialyzer shell clean

compile:
	@$(REBAR) compile

dialyzer: $(DEPSOLVER_PLT)
	@dialyzer --plt $(DEPSOLVER_PLT) --src ./src

$(DEPSOLVER_PLT):
	@dialyzer --build_plt --output_plt $(DEPSOLVER_PLT) --apps erts kernel stdlib crypto compiler

shell: compile
	@$(REBAR) shell

clean:
	@$(REBAR) clean
