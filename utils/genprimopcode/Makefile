genprimopcode: ParserM.hs Parser.hs Lexer.hs Syntax.hs Main.hs
	ghc --make Main.hs -o genprimopcode

Parser.hs: Parser.y
	happy Parser.y

Lexer.hs: Lexer.x
	alex Lexer.x
