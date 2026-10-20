# CMake generated Testfile for 
# Source directory: /home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/source/MPI4
# Build directory: /home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/mpi4/MPI4
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test([=[mpi4_bridge_thread]=] "/usr/bin/mpiexec" "-n" "2" "/home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/mpi4/MPI4/bpack_mpi4_test")
set_tests_properties([=[mpi4_bridge_thread]=] PROPERTIES  PROCESSORS "2" TIMEOUT "60" _BACKTRACE_TRIPLES "/home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/source/MPI4/CMakeLists.txt;28;add_test;/home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/source/MPI4/CMakeLists.txt;0;")
add_test([=[mpi4_bridge_basic]=] "/usr/bin/mpiexec" "-n" "2" "/home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/mpi4/MPI4/bpack_mpi4_test" "init")
set_tests_properties([=[mpi4_bridge_basic]=] PROPERTIES  PROCESSORS "2" TIMEOUT "60" _BACKTRACE_TRIPLES "/home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/source/MPI4/CMakeLists.txt;28;add_test;/home/runner/work/ButterflyPACK/ButterflyPACK/.cabi-validation/source/MPI4/CMakeLists.txt;0;")
