(use-package gnugo ; Game of Go
	:init	(setq gnugo-program "/usr/local/bin/gnugo")
	:config	(easy-menu-add-item  nil '("tools" "games") ["Go" gnugo t]))
