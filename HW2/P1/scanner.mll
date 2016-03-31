{ open Parser }

let digit = ['0' - '9']
let fraction = ['.']digit*
let exponent = ['e' 'E']['+' '-']?['0' - '9']+
let right_side = ((fraction | exponent) | (fraction exponent))

let my_float = (digit+ right_side) | ('.' digit+ right_side?)

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf }
	| "/*" { comment lexbuf }
	| my_float as lxm { FLOAT(lxm) }
	| eof { EOF }

and comment = parse 
	"*/" { token lexbuf }
	| _ { comment lexbuf }
