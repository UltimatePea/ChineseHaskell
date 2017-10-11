demo:
	stack build
	cat test/chinese.chs
	stack exec chinesehaskell-exe < test/chinese.chs > test/chinese_compiled.hs 
	cat test/chinese_compiled.hs
	ghci test/chinese_compiled.hs 
	rm test/chinese_compiled.hs
