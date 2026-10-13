program RandomSeedTest
   use iso_fortran_env, only: int64
   use d_MISC_Utilities, only: init_d => d_init_random_seed
   use z_MISC_Utilities, only: init_z => z_init_random_seed
   use s_MISC_Utilities, only: init_s => s_init_random_seed
   use c_MISC_Utilities, only: init_c => c_init_random_seed
   implicit none
#ifdef HAVE_MPI
   include 'mpif.h'
   integer :: ierr
#endif
   integer :: rank, nprocs

   rank = 0
   nprocs = 1
   call check_seed(init_d)
#ifdef HAVE_MPI
   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
#endif
   call check_seed(init_d)
   call check_seed(init_z)
   call check_seed(init_s)
   call check_seed(init_c)
#ifdef HAVE_MPI
   call MPI_Finalize(ierr)
#endif

contains

   subroutine check_seed(initialize)
      interface
         subroutine initialize()
         end subroutine initialize
      end interface
      integer, allocatable :: seed(:), repeated(:)
      integer :: n, expected
      integer(int64) :: base
      real(kind=8) :: values(16), repeated_values(16)
#ifdef HAVE_MPI
      integer, allocatable :: rank_seeds(:)
      integer :: i
      logical :: started
#endif

      call random_seed(size=n)
      allocate(seed(n), repeated(n))
      call initialize()
      call random_seed(get=seed)
      call random_number(values)
      call initialize()
      call random_seed(get=repeated)
      call random_number(repeated_values)
      if (any(seed /= repeated)) error stop 'Seed is not repeatable'
      if (any(values /= repeated_values)) error stop 'Random sequence is not repeatable'

      base = 104729_int64 + int(rank, int64)
      expected = int(mod(mod(base*279470273_int64, 4294967291_int64), int(huge(0), int64)))
      if (seed(1) /= expected) error stop 'Seed does not use fixed base plus rank'
#ifdef HAVE_MPI
      call MPI_Initialized(started, ierr)
      if (started) then
         allocate(rank_seeds(nprocs))
         call MPI_Allgather(seed(1), 1, MPI_INTEGER, rank_seeds, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)
         do i = 1, nprocs
            if (i == rank + 1) cycle
            if (seed(1) == rank_seeds(i)) error stop 'MPI ranks have identical seeds'
         end do
      endif
#endif
   end subroutine check_seed

end program RandomSeedTest
