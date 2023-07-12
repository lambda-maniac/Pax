module List where

import Pax.Base

integer =
    tokenized $ some $ oneof digits
lbr =
    tokenized $ string "["
rbr =
    tokenized $ string "]"
comma =
    tokenized $ string ","

list  =  lbr <&> list_items <&> rbr
     <|> lbr <&> rbr
     <|> eof

list_items  =  integer <&> comma <&> list_items
           <|> integer
           <|> eof
