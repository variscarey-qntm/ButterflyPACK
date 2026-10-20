add_library(bpack_mpi4 OBJECT bpack_mpi4.c bpack_mpi4_constants.f90)
set_target_properties(bpack_mpi4 PROPERTIES
  POSITION_INDEPENDENT_CODE ON
  C_STANDARD 11
  C_STANDARD_REQUIRED ON
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(bpack_mpi4 PRIVATE MPI::MPI_C MPI::MPI_Fortran)

include(GNUInstallDirs)
install(FILES bpack_mpi4.fi DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/"
  DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
  FILES_MATCHING PATTERN "*.mod")

if(BUILD_TESTING)
  find_package(Threads REQUIRED)
  add_executable(bpack_mpi4_test test_mpi4.c test_mpi4.f90
    $<TARGET_OBJECTS:bpack_mpi4>)
  add_dependencies(bpack_mpi4_test bpack_mpi4)
  target_include_directories(bpack_mpi4_test PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  target_link_libraries(bpack_mpi4_test PRIVATE MPI::MPI_C MPI::MPI_Fortran Threads::Threads)
  foreach(initialization IN ITEMS thread basic)
    if(initialization STREQUAL "basic")
      set(init_argument init)
    else()
      set(init_argument)
    endif()
    add_test(NAME mpi4_bridge_${initialization}
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2
        ${MPIEXEC_PREFLAGS} $<TARGET_FILE:bpack_mpi4_test>
        ${MPIEXEC_POSTFLAGS} ${init_argument})
    set_tests_properties(mpi4_bridge_${initialization} PROPERTIES
      PROCESSORS 2 TIMEOUT 60)
  endforeach()
endif()
