module Calculator where

import Pax.Base

int' =
    tokenized $ some $ oneof digits
mul' =
    tokenized $ string "*"
div' =
    tokenized $ string "/"
add' =
    tokenized $ string "+"
sub' =
    tokenized $ string "-"
lpr' =
    tokenized $ string "("
rpr' =
    tokenized $ string ")"

expression  =  term <&> mul' <&> expression
           <|> term <&> div' <&> expression
           <|> term

term        =  fact <&> add' <&> term
           <|> fact <&> sub' <&> term
           <|> fact

fact        =  lpr' *> expression <* rpr'
           <|> int'
           <|> eof

-- No negatives because I'm lazy (Like haskell (No interpreteer also (xd))).
