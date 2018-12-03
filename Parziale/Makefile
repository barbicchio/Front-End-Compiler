all:
	happy -gca ParLua.y
	alex -g LexLua.x
	ghc --make TestLua.hs -o TestLua

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocLua.* LexLua.* ParLua.* LayoutLua.* SkelLua.* PrintLua.* TestLua.* AbsLua.* TestLua ErrM.* SharedString.* ComposOp.* Lua.dtd XMLLua.* Makefile*
	

