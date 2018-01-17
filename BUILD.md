# Building Skalpel

A (hopefully) short and easy step by step to building Skalpel from source.

### Dependencies

- [`autoconf`](http://www.gnu.org/software/autoconf/autoconf.html)
- [`mlton`](https://mlton.org) (>= 20130715) (Skalpel requires mllpt-lib)
- [`polyml`](http://www.polyml.org) (Recommended for development builds)

## Building

1. Clone this repository: `git clone https://github.com/ultra-group/skalpel`
2. Move to source root: `cd skalpel/analysis-engines/standard-ml`
3. Run autoconf: `autoconf configure.ac`
4. Run configuration script: `./configure`
    - You can pass `./configure --prefix=<path>` to change the installation location
5. Build! `make mlton-bin`
6. Install: `make install`
