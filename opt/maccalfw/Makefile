
all: build

build:
	swiftc -Xcc -fmodule-map-file=src/module.modulemap \
		-I/Applications/Emacs.app/Contents/Resources/include \
		src/EmacsUtil.swift \
		src/MacCalfw.swift \
		-O -emit-library -o libmaccalfw.dylib

test:
	emacs -Q --batch --load test.el
