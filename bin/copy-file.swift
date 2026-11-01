#!/usr/bin/env swift

// copy-file.swift — Copy a file reference to the macOS clipboard.
//
// Used by cpj/dired-copy-file in Emacs to copy files from Dired
// for pasting into Finder, ChatGPT, and other macOS applications.
//
// Compile from ~/.emacs.d/bin:
//
//   swiftc -O copy-file.swift -o copy-file
//
// Recompile after modifying this source.
//
// Requires Apple's Swift compiler (Xcode Command Line Tools).
// The compiled executable is invoked by Emacs; the Swift source
// is retained for maintenance and recompilation.

import AppKit
import Foundation

guard CommandLine.arguments.count > 1 else {
    fputs("Usage: copy-file.swift FILE\n", stderr)
    exit(1)
}

let fileURL = URL(fileURLWithPath: CommandLine.arguments[1])

let pasteboard = NSPasteboard.general
pasteboard.clearContents()

guard pasteboard.writeObjects([fileURL as NSURL]) else {
    fputs("Failed to copy file to clipboard\n", stderr)
    exit(1)
}

// Allow macOS time to obtain the pasteboard data before exiting.
// Without this delay, the compiled executable reports success,
// but Finder and ChatGPT cannot paste the file.
// The interpreted Swift script does not exhibit this problem.

Thread.sleep(forTimeInterval: 0.25)
