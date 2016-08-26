# Parse transform scripts
[![Build Status](https://travis-ci.org/eltex-ecss/pt_scripts.svg?branch=master)](https://travis-ci.org/eltex-ecss/pt_scripts)

## Includes:
* pt_lazy_case.erl    - case unfolds from a tuple
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

For work pt_lazy_case need to connect -include_lib("pt_scripts/include/pt_lazy_case.hrl") and to use macro ?lazy_case before the next type of structures
```
X = case {...} of ... end
or
case {...} of ... end
```

When using pt_lazy_case has the following limitations:
```
1) As the case is allowed only tuple
2) For variables, the Guard is not recommended, and for the tuple, is prohibited.
3) Matching also should be consistent, for example
    case {A1, A2} of
        {line, 'UserStr'} ->
            ok1;
        {message, 'UserStr'}  ->
            ok2;
        {message, 'UserStrLine'} ->
            ok3;
        {line, 'UserStrLine'} ->
            ok4;
        {_, 'UserStr'} ->
            ok5;
        {_, 'UserStrLine'} ->
            ok6;
        _ ->
            ok7
    end.
4) This module is not stable and requires the use only when tuple is composed of slow functions
(use at your own risk)
```