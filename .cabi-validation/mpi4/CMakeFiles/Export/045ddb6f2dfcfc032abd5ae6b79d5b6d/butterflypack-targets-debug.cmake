#----------------------------------------------------------------
# Generated CMake target import file for configuration "Debug".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "ButterflyPACK::butterflypack" for configuration "Debug"
set_property(TARGET ButterflyPACK::butterflypack APPEND PROPERTY IMPORTED_CONFIGURATIONS DEBUG)
set_target_properties(ButterflyPACK::butterflypack PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_DEBUG "C;CXX;Fortran"
  IMPORTED_LOCATION_DEBUG "${_IMPORT_PREFIX}/lib/libbutterflypack.a"
  )

list(APPEND _cmake_import_check_targets ButterflyPACK::butterflypack )
list(APPEND _cmake_import_check_files_for_ButterflyPACK::butterflypack "${_IMPORT_PREFIX}/lib/libbutterflypack.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
