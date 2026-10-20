file(REMOVE_RECURSE
  "libbutterflypack.a"
  "libbutterflypack.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang C CXX Fortran)
  include(CMakeFiles/butterflypack.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
