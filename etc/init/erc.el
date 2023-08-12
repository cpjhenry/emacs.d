(load "rc/erc" 'noerror) ; irc config
(easy-menu-add-item  nil '("tools")	["IRC with ERC" erc t])
(bind-key "C-c e" 'erc)
