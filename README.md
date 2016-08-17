# Parse transform scripts
[![Build Status](https://travis-ci.org/eltex-ecss/pt_scripts.svg?branch=master)](https://travis-ci.org/eltex-ecss/pt_scripts)

## Includes:
* pt_const_fun.erl    - replaces the function with constant data on the results of their work
* pt_build_info.erl   - add get_info() returned git info for build project
* pt_fabric.erl       - parametrized modules
* pt_fun_guards.erl   - allow lists:member in guards test
* pt_guard.erl        - allow user function in guards test
* pt_gen_proxy.erl    - generated proxy modules
* pt_macro.erl        - add macro for func,args name and compile time regexp
* pt_pmodule.erl      - parametrized modules
* pt_recompilable.erl - mutable modules
* pt_records.erl      - access to records field in compile time
* pt_str_parser.erl   - add string:match for easy regexp
* pt_versioned.erl    - add build_info function
* pt_pp.erl

## Usages:
For usage pt_script include needs header:
```
-include_lib("pt_scripts/include/pt_records.hrl").
```

Or add compile options:
```
-compile({parse_transform, pt_records}).
```
