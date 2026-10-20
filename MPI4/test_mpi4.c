#include <mpi.h>
#include <stdint.h>
#include <pthread.h>

typedef int64_t i8;
extern i8 bpack_mpi_status_ignore[], bpack_mpi_statuses_ignore[];
void bpack_mpi_iallgatherv_(void *, const i8 *, const i8 *, void *, const i8 *,
                          const i8 *, const i8 *, const i8 *, i8 *, i8 *);
void bpack_mpi_wait_(i8 *, i8 *, i8 *);
void bpack_mpi_waitall_(const i8 *, i8 *, i8 *, i8 *);
void bpack_mpi_waitany_(const i8 *, i8 *, i8 *, i8 *, i8 *);

struct worker {
    MPI_Comm comm;
    int id, rank, success;
};

static void *thread_checks(void *arg)
{
    struct worker *w = arg;
    i8 c = MPI_Comm_c2f(w->comm), type = MPI_Type_c2f(MPI_INT64_T);
    i8 one = 1, two = 2, counts[2] = {1, 1}, displs[2] = {0, 1};
    w->success = 1;
    for (int j = 0; j < 300; ++j) {
        i8 base = 1000 * j + 100 * w->id, x = base + w->rank;
        i8 recv[2][2] = {{-1, -1}, {-1, -1}}, requests[2], ierr, index;
        for (int k = 0; k < 2; ++k) {
            bpack_mpi_iallgatherv_(&x, &one, &type, recv[k], counts, displs,
                                   &type, &c, &requests[k], &ierr);
            if (ierr != MPI_SUCCESS) MPI_Abort(w->comm, (int)ierr);
        }
        if (j % 3 == 0) {
            for (int k = 0; k < 2; ++k) {
                bpack_mpi_wait_(&requests[k], bpack_mpi_status_ignore, &ierr);
                if (ierr != MPI_SUCCESS) MPI_Abort(w->comm, (int)ierr);
            }
        } else {
            if (j % 3 == 1) {
                bpack_mpi_waitany_(&two, requests, &index, bpack_mpi_status_ignore, &ierr);
                if (ierr != MPI_SUCCESS) MPI_Abort(w->comm, (int)ierr);
            }
            bpack_mpi_waitall_(&two, requests, bpack_mpi_statuses_ignore, &ierr);
            if (ierr != MPI_SUCCESS) MPI_Abort(w->comm, (int)ierr);
        }
        for (int k = 0; k < 2; ++k)
            if (recv[k][0] != base || recv[k][1] != base + 1) w->success = 0;
    }
    return NULL;
}

i8 bpack_mpi_test_threads(void)
{
    enum { NWORKERS = 4 };
    struct worker workers[NWORKERS];
    pthread_t threads[NWORKERS];
    int rank, success = 1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    /* Distinct communicators keep concurrent collective initiation legal. */
    for (int j = 0; j < NWORKERS; ++j) {
        MPI_Comm_dup(MPI_COMM_WORLD, &workers[j].comm);
        workers[j].id = j;
        workers[j].rank = rank;
    }
    for (int j = 0; j < NWORKERS; ++j)
        if (pthread_create(&threads[j], NULL, thread_checks, &workers[j]) != 0)
            MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER);
    for (int j = 0; j < NWORKERS; ++j) {
        pthread_join(threads[j], NULL);
        success = success && workers[j].success;
        MPI_Comm_free(&workers[j].comm);
    }
    return success;
}

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

void bpack_mpi_test_errors_return(void)
{
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
}
