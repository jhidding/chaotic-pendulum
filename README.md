# chaotic-pendulum
Demo for Entangled with a chaotic pendulum in PureScript. This is a physics demo that runs completely in the browser.

## Requirements
To develop on this:

- Node >=12
  - PureScript >=0.13 (`npm install -g purs`), the PureScript compiler
  - spago (`npm install -g spago`), the build system for PureScript
  - browser-sync (`npm install -g browser-sync`), for watching
- Pandoc
- [Entangled](https://entangled.github.io/) to do literate programming
- Entangled filters (`pip install entangled-filters`)
- GNU Make
  - tmux, for watching
  - inotifywait, for watching

## Building/Running
Build the static page with

    make site
 
While developing run

    make watch

to get live browser updates
