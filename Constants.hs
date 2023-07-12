module Pax.Constants where

digits        = "1234567890"                 :: String
alpha_lower   = "abcdefghijklmnopqrstuvwxyz" :: String
alpha_upper   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String
alpha         = alpha_lower   ++ alpha_upper :: String
alpha_numeric = alpha         ++ digits      :: String
