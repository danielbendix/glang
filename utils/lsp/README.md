# Language server

This directory contains a very simple language server implementation using `pygls`, useful for shortening the edit-test loop.

## How to use

If the compiler executable is not available in your `$PATH`, the path to it needs to be provided as a command line argument.

You need to set up a virtual environment for the lsp, and install the requirements before use.

To add the LSP to an editor configuration, it would have to be invoked like:

`$LSP_DIRECTORY/.venv/bin/python3 $LSP_DIRECTORY/main.py /path/to/compiler/binary`

## Potential improvements

The implementation could use `watchdog` to watch the compiler binary for changes, and update diagnostics for any open file when the compiler binary is updated.
