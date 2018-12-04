base = Lua
HAPPY = happy -gca
ALEX  = alex -g
COMP   = ghc --make

tocompile = Par$(base).hs Lex$(base).hs Abs$(base).hs Print$(base).hs Typechecker$(base).hs Tac$(base).hs Tacpp$(base).hs ErrM.hs

Test$(base) : Test$(base).hs $(tocompile)
	$(COMP) $< -o $@

.PHONY : clean
clean:
	-rm -fv *.hi *.o *.log *.info

distclean: clean
	-rm -fv Test$(base) Lex$(base).hs Par$(base).hs

demo: Test$(base)
	./Test$(base) ./Tests/test3.lua
.PHONY:demo-error

demo-error: Test$(base)
		./Test$(base) ./TestErrors/test_error0.lua


Par$(base).hs: Par$(base).y
	$(HAPPY) $<

Lex$(base).hs: Lex$(base).x
	$(ALEX) $<

	

