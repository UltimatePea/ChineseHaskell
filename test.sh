stack exec chinesehaskell-exe < test/chinese.chs > test/chinese_compiled.hs 
ghci test/chinese_compiled.hs 
rm test/chinese_compiled.hs
