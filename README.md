# Belgian companies listing

The Belgian Crossroads Bank for Enterprises (CBE) is "a database owned by the
FPS Economy containing all the basic data on companies and their business
units".

The CBE provides a subset of its database as CSV files, known as "Open Data
files". These files can be manually downloaded from a website after registering
with the CBE, which includes accepting the Open Data license and specifying the
intended use of the files.

This Git repository provides an easy-to-use command-line tool to automate the
download of these CSV files, allowing you to schedule downloads and ensure that
you always have the latest data available.

## Building

```
$ nix-build -A binaries
```

## Commands

```
$ enterprises --help
enterprises - Download Open Data files from the CBE.

Usage: enterprises COMMAND

  This program assists in downloading CSV files from the CBE Open Data pages.

Available options:
  -h,--help                Show this help text

Available commands:
  download-archives        Download missing archives
  list-archives            List downloaded and/or remote archives
  download-index           Download an index of available remote archives
  parse-index              Parse a downloaded index of available remote archives
  login                    Verify the provided username and password
```

## Acknowledgment

This software is developed by Hypered SRL for the Refli Software-as-a-Service
web application. It is provided "as is" under the 2-Clause BSD license. For
support, custom development, or to explore how we can help with your specific
software needs, please contact Hypered SRL.
