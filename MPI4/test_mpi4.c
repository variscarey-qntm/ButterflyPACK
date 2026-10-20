#include <mpi.h>
#include <stdint.h>

int64_t bpack_mpi_test_zero_type(void)
{
    MPI_Datatype type;
    MPI_Type_contiguous(0, MPI_BYTE, &type);
    MPI_Type_commit(&type);
    return MPI_Type_c2f(type);
}

void bpack_mpi_test_free_type(int64_t *handle)
{
    MPI_Datatype type = MPI_Type_f2c((MPI_Fint)*handle);
    MPI_Type_free(&type);
    *handle = MPI_Type_c2f(type);
}
