# SSOSVM

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/SSOSVM
* Date/Publication: 2019-05-06 09:10:03 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"SSOSVM")` for more info

</details>

## In both

*   checking whether package ‘SSOSVM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/thomas/Dropbox/GitHub/gganimate/revdep/checks.noindex/SSOSVM/new/SSOSVM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SSOSVM’ ...
** package ‘SSOSVM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c main.cpp -o main.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o SSOSVM.so RcppExports.o main.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [SSOSVM.so] Error 1
ERROR: compilation failed for package ‘SSOSVM’
* removing ‘/Users/thomas/Dropbox/GitHub/gganimate/revdep/checks.noindex/SSOSVM/new/SSOSVM.Rcheck/SSOSVM’

```
### CRAN

```
* installing *source* package ‘SSOSVM’ ...
** package ‘SSOSVM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/Rcpp/include" -I"/Users/thomas/Dropbox/GitHub/gganimate/revdep/library.noindex/SSOSVM/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c main.cpp -o main.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o SSOSVM.so RcppExports.o main.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [SSOSVM.so] Error 1
ERROR: compilation failed for package ‘SSOSVM’
* removing ‘/Users/thomas/Dropbox/GitHub/gganimate/revdep/checks.noindex/SSOSVM/old/SSOSVM.Rcheck/SSOSVM’

```
