# Contributing
## Inline comments
This project employs inline comments to express issues.
- TODO-soon: Refers to issues that are prioritised and should be addressed _soon_.
- TODO-someday: Refers to non-urgent issues that are nice-to-have and may be addressed _someday_.

## Commit style
Each commit represents an atomic change. Every commit should pass all checks
and build successfully. A commit should not be made just because the codebase
compiles, but should be made when an objective is fully achieved and addressed.

A commit description should also be created to describe what changed in the code
in detail. It should be written in a way that a person in the future is able to
answer the following:
- What was the commit created for?
- Why was it created?
- How was it done?

## Commands
- To build the project: `dune build`
- To run the formatter: `dune fmt`
- To run tests and accept changes: `dune runtest --auto-promote`

All 3 commands should succeed silently on all commits.
