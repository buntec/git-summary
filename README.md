# git-summary

A command-line tool that displays a concise status summary of all git repos under a
given root (which defaults to the current working directory).

Use `git-summary --help` for a list of options.

The tool is written in Haskell and ships as static binaries for Linux, macOS and Windows.
You can download the latest version from the [release page](https://github.com/buntec/git-summary/releases).

The only prerequisite is git itself, which must be on your PATH.

To install, simply download and extract the binary for your OS and copy it to a location on your PATH.
On macOS, running it the first time will most likely require manual unblocking under
System Preferences -> Security & Privacy.

Heavily inspired by this [bash script](https://github.com/MirkoLedda/git-summary) of the same name.

