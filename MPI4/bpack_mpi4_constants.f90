! Keep mpif.h outside the preprocessing mappings: its handles are MPI_Fint
! values, whereas the application uses default INTEGER/LOGICAL(kind=8).
module bpack_mpi4_constants
    use, intrinsic :: iso_c_binding, only: c_int64_t
    implicit none
    include 'mpif.h'

    integer(c_int64_t), bind(C, name="bpack_mpi_in_place") :: bpack_mpi_in_place = 0
    integer(c_int64_t), bind(C, name="bpack_mpi_bottom") :: bpack_mpi_bottom = 0
    integer(c_int64_t), bind(C, name="bpack_mpi_status_ignore") :: bpack_mpi_status_ignore(MPI_STATUS_SIZE) = 0
    integer(c_int64_t), bind(C, name="bpack_mpi_statuses_ignore") :: bpack_mpi_statuses_ignore(MPI_STATUS_SIZE, 1) = 0
    integer(c_int64_t), bind(C, name="bpack_mpi_status_size") :: bpack_mpi_status_size = MPI_STATUS_SIZE
    double precision, external :: bpack_mpi_wtime, bpack_mpi_wtick
end module bpack_mpi4_constants
