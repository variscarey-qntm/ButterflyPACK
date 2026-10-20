/*
 * Legacy Fortran entry points for gfortran -fdefault-integer-8.
 * This file is compiled once, without the Fortran preprocessing mappings.
 */
#include <mpi.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>
#include <stdatomic.h>

#if MPI_VERSION < 4
#error "ButterflyPACK MPI4 requires MPI-4 large-count C bindings"
#endif

typedef int64_t i8;
extern i8 bpack_mpi_in_place, bpack_mpi_bottom;
extern i8 bpack_mpi_status_ignore[], bpack_mpi_statuses_ignore[];
extern i8 bpack_mpi_status_size;

static MPI_Comm comm(const i8 *f) { return MPI_Comm_f2c((MPI_Fint)*f); }
static MPI_Datatype datatype(const i8 *f) { return MPI_Type_f2c((MPI_Fint)*f); }
static MPI_Op op(const i8 *f) { return MPI_Op_f2c((MPI_Fint)*f); }
static void *buffer(void *p)
{
    if (p == &bpack_mpi_in_place) return MPI_IN_PLACE;
    if (p == &bpack_mpi_bottom) return MPI_BOTTOM;
    return p;
}

static int ignore_status(const i8 *p)
{
    return p == bpack_mpi_status_ignore || p == bpack_mpi_statuses_ignore;
}

static void status_out(const MPI_Status *s, i8 *f)
{
    if (!ignore_status(f)) {
        MPI_Fint native[bpack_mpi_status_size];
        MPI_Status_c2f(s, native);
        for (i8 j = 0; j < bpack_mpi_status_size; ++j) f[j] = native[j];
    }
}

static void status_in(const i8 *f, MPI_Status *s)
{
    MPI_Fint native[bpack_mpi_status_size];
    for (i8 j = 0; j < bpack_mpi_status_size; ++j) native[j] = (MPI_Fint)f[j];
    MPI_Status_f2c(native, s);
}

static int int_value(i8 value, int *out)
{
    if (value < INT_MIN || value > INT_MAX) return MPI_ERR_ARG;
    *out = (int)value;
    return MPI_SUCCESS;
}

#define INT_ARG(name) \
    int c_##name; \
    if ((*ierr = int_value(*name, &c_##name)) != MPI_SUCCESS) return

/*
 * Nonblocking vector collectives retain count/displacement arrays until the
 * request completes. Keep the converted arrays alive, including with
 * MPI_THREAD_MULTIPLE.
 */
struct vectors {
    MPI_Count *counts;
    MPI_Aint *displs;
    MPI_Fint request;
    struct vectors *next;
};
static struct vectors *pending;
static atomic_flag pending_lock = ATOMIC_FLAG_INIT;

static void lock_pending(void)
{
    while (atomic_flag_test_and_set_explicit(&pending_lock, memory_order_acquire)) {}
}
static void unlock_pending(void)
{
    atomic_flag_clear_explicit(&pending_lock, memory_order_release);
}
static void free_vectors(struct vectors *v)
{
    if (v) { free(v->counts); free(v->displs); free(v); }
}
static void retain_vectors(struct vectors *v, MPI_Request request)
{
    if (request == MPI_REQUEST_NULL) { free_vectors(v); return; }
    v->request = MPI_Request_c2f(request);
    lock_pending();
    v->next = pending;
    pending = v;
    unlock_pending();
}
static void release_vectors(MPI_Fint request)
{
    struct vectors *found = NULL;
    lock_pending();
    for (struct vectors **p = &pending; *p; p = &(*p)->next) {
        if ((*p)->request == request) {
            found = *p;
            *p = found->next;
            break;
        }
    }
    unlock_pending();
    free_vectors(found);
}

static int vector_size(MPI_Comm c, int *n)
{
    int inter, err = MPI_Comm_test_inter(c, &inter);
    if (err != MPI_SUCCESS) return err;
    return inter ? MPI_Comm_remote_size(c, n) : MPI_Comm_size(c, n);
}
static int root_vectors(MPI_Comm c, int root, int *needed)
{
    int inter, rank, err = MPI_Comm_test_inter(c, &inter);
    if (err != MPI_SUCCESS) return err;
    if (inter) { *needed = root == MPI_ROOT; return MPI_SUCCESS; }
    err = MPI_Comm_rank(c, &rank);
    *needed = rank == root;
    return err;
}
static int make_vectors(MPI_Comm c, const i8 *counts, const i8 *displs,
                        struct vectors **out)
{
    int n, err = vector_size(c, &n);
    *out = NULL;
    if (err != MPI_SUCCESS) return err;
    struct vectors *v = calloc(1, sizeof(*v));
    if (!v) return MPI_ERR_NO_MEM;
    v->counts = malloc((size_t)(n ? n : 1) * sizeof(*v->counts));
    if (displs) v->displs = malloc((size_t)(n ? n : 1) * sizeof(*v->displs));
    if (!v->counts || (displs && !v->displs)) { free_vectors(v); return MPI_ERR_NO_MEM; }
    for (int j = 0; j < n; ++j) {
        v->counts[j] = (MPI_Count)counts[j];
        if (displs) v->displs[j] = (MPI_Aint)displs[j];
    }
    *out = v;
    return MPI_SUCCESS;
}

void bpack_mpi_init_(i8 *ierr) { *ierr = MPI_Init(NULL, NULL); }
void bpack_mpi_init_thread_(const i8 *required, i8 *provided, i8 *ierr)
{
    INT_ARG(required);
    int value = 0;
    *ierr = MPI_Init_thread(NULL, NULL, c_required, &value);
    *provided = value;
}
void bpack_mpi_finalize_(i8 *ierr) { *ierr = MPI_Finalize(); }

void bpack_mpi_comm_rank_(const i8 *c, i8 *rank, i8 *ierr)
{
    int value = 0;
    *ierr = MPI_Comm_rank(comm(c), &value);
    *rank = value;
}
void bpack_mpi_comm_size_(const i8 *c, i8 *size, i8 *ierr)
{
    int value = 0;
    *ierr = MPI_Comm_size(comm(c), &value);
    *size = value;
}
void bpack_mpi_comm_group_(const i8 *c, i8 *group, i8 *ierr)
{
    MPI_Group value = MPI_GROUP_NULL;
    *ierr = MPI_Comm_group(comm(c), &value);
    *group = MPI_Group_c2f(value);
}
void bpack_mpi_comm_create_(const i8 *c, const i8 *group, i8 *result, i8 *ierr)
{
    MPI_Comm value = MPI_COMM_NULL;
    *ierr = MPI_Comm_create(comm(c), MPI_Group_f2c((MPI_Fint)*group), &value);
    *result = MPI_Comm_c2f(value);
}
void bpack_mpi_comm_split_(const i8 *c, const i8 *color, const i8 *key, i8 *result, i8 *ierr)
{
    INT_ARG(color);
    INT_ARG(key);
    MPI_Comm value = MPI_COMM_NULL;
    *ierr = MPI_Comm_split(comm(c), c_color, c_key, &value);
    *result = MPI_Comm_c2f(value);
}
void bpack_mpi_comm_free_(i8 *c, i8 *ierr)
{
    MPI_Comm value = comm(c);
    *ierr = MPI_Comm_free(&value);
    *c = MPI_Comm_c2f(value);
}
void bpack_mpi_comm_get_parent_(i8 *c, i8 *ierr)
{
    MPI_Comm value = MPI_COMM_NULL;
    *ierr = MPI_Comm_get_parent(&value);
    *c = MPI_Comm_c2f(value);
}
void bpack_mpi_comm_disconnect_(i8 *c, i8 *ierr)
{
    MPI_Comm value = comm(c);
    *ierr = MPI_Comm_disconnect(&value);
    *c = MPI_Comm_c2f(value);
}
void bpack_mpi_group_incl_(const i8 *group, const i8 *n, const i8 *ranks, i8 *result, i8 *ierr)
{
    INT_ARG(n);
    if (c_n < 0) { *ierr = MPI_ERR_COUNT; return; }
    int *values = malloc((size_t)(c_n ? c_n : 1) * sizeof(*values));
    if (!values) { *ierr = MPI_ERR_NO_MEM; return; }
    for (int j = 0; j < c_n; ++j) {
        if ((*ierr = int_value(ranks[j], &values[j])) != MPI_SUCCESS) { free(values); return; }
    }
    MPI_Group value = MPI_GROUP_NULL;
    *ierr = MPI_Group_incl(MPI_Group_f2c((MPI_Fint)*group), c_n, values, &value);
    *result = MPI_Group_c2f(value);
    free(values);
}
void bpack_mpi_group_free_(i8 *group, i8 *ierr)
{
    MPI_Group value = MPI_Group_f2c((MPI_Fint)*group);
    *ierr = MPI_Group_free(&value);
    *group = MPI_Group_c2f(value);
}
void bpack_mpi_barrier_(const i8 *c, i8 *ierr) { *ierr = MPI_Barrier(comm(c)); }

void bpack_mpi_bcast_(void *buf, const i8 *count, const i8 *type,
                     const i8 *root, const i8 *c, i8 *ierr)
{
    INT_ARG(root);
    *ierr = MPI_Bcast_c(buffer(buf), (MPI_Count)*count, datatype(type), c_root, comm(c));
}
void bpack_mpi_allreduce_(void *send, void *recv, const i8 *count, const i8 *type,
                         const i8 *operation, const i8 *c, i8 *ierr)
{
    *ierr = MPI_Allreduce_c(buffer(send), buffer(recv), (MPI_Count)*count,
                            datatype(type), op(operation), comm(c));
}
void bpack_mpi_reduce_(void *send, void *recv, const i8 *count, const i8 *type,
                      const i8 *operation, const i8 *root, const i8 *c, i8 *ierr)
{
    INT_ARG(root);
    *ierr = MPI_Reduce_c(buffer(send), buffer(recv), (MPI_Count)*count,
                         datatype(type), op(operation), c_root, comm(c));
}

#define FIXED_COLLECTIVE(name, api) \
void bpack_mpi_##name##_(void *send, const i8 *sc, const i8 *st, void *recv, \
                         const i8 *rc, const i8 *rt, const i8 *c, i8 *ierr) \
{ \
    *ierr = api(buffer(send), (MPI_Count)*sc, datatype(st), buffer(recv), \
                (MPI_Count)*rc, datatype(rt), comm(c)); \
}
FIXED_COLLECTIVE(allgather, MPI_Allgather_c)
FIXED_COLLECTIVE(alltoall, MPI_Alltoall_c)

#define IFIXED_COLLECTIVE(name, api) \
void bpack_mpi_##name##_(void *send, const i8 *sc, const i8 *st, void *recv, \
                         const i8 *rc, const i8 *rt, const i8 *c, i8 *request, i8 *ierr) \
{ \
    MPI_Request value = MPI_REQUEST_NULL; \
    *ierr = api(buffer(send), (MPI_Count)*sc, datatype(st), buffer(recv), \
                (MPI_Count)*rc, datatype(rt), comm(c), &value); \
    *request = MPI_Request_c2f(value); \
}
IFIXED_COLLECTIVE(iallgather, MPI_Iallgather_c)
IFIXED_COLLECTIVE(ialltoall, MPI_Ialltoall_c)

void bpack_mpi_gather_(void *send, const i8 *sc, const i8 *st, void *recv,
                      const i8 *rc, const i8 *rt, const i8 *root, const i8 *c, i8 *ierr)
{
    INT_ARG(root);
    *ierr = MPI_Gather_c(buffer(send), (MPI_Count)*sc, datatype(st), buffer(recv),
                         (MPI_Count)*rc, datatype(rt), c_root, comm(c));
}
void bpack_mpi_allgatherv_(void *send, const i8 *sc, const i8 *st, void *recv,
                         const i8 *rc, const i8 *displs, const i8 *rt, const i8 *c, i8 *ierr)
{
    struct vectors *v;
    MPI_Comm value = comm(c);
    if ((*ierr = make_vectors(value, rc, displs, &v)) != MPI_SUCCESS) return;
    *ierr = MPI_Allgatherv_c(buffer(send), (MPI_Count)*sc, datatype(st), buffer(recv),
                             v->counts, v->displs, datatype(rt), value);
    free_vectors(v);
}
void bpack_mpi_iallgatherv_(void *send, const i8 *sc, const i8 *st, void *recv,
                          const i8 *rc, const i8 *displs, const i8 *rt,
                          const i8 *c, i8 *request, i8 *ierr)
{
    struct vectors *v;
    MPI_Comm value = comm(c);
    MPI_Request req = MPI_REQUEST_NULL;
    if ((*ierr = make_vectors(value, rc, displs, &v)) != MPI_SUCCESS) return;
    *ierr = MPI_Iallgatherv_c(buffer(send), (MPI_Count)*sc, datatype(st), buffer(recv),
                              v->counts, v->displs, datatype(rt), value, &req);
    *request = MPI_Request_c2f(req);
    if (*ierr == MPI_SUCCESS) retain_vectors(v, req);
    else free_vectors(v);
}
void bpack_mpi_gatherv_(void *send, const i8 *sc, const i8 *st, void *recv,
                       const i8 *rc, const i8 *displs, const i8 *rt,
                       const i8 *root, const i8 *c, i8 *ierr)
{
    INT_ARG(root);
    struct vectors *v = NULL;
    MPI_Comm value = comm(c);
    int needed;
    if ((*ierr = root_vectors(value, c_root, &needed)) != MPI_SUCCESS) return;
    if (needed && (*ierr = make_vectors(value, rc, displs, &v)) != MPI_SUCCESS) return;
    *ierr = MPI_Gatherv_c(buffer(send), (MPI_Count)*sc, datatype(st), buffer(recv),
                          v ? v->counts : NULL, v ? v->displs : NULL, datatype(rt), c_root, value);
    free_vectors(v);
}
void bpack_mpi_scatterv_(void *send, const i8 *sc, const i8 *displs, const i8 *st,
                        void *recv, const i8 *rc, const i8 *rt,
                        const i8 *root, const i8 *c, i8 *ierr)
{
    INT_ARG(root);
    struct vectors *v = NULL;
    MPI_Comm value = comm(c);
    int needed;
    if ((*ierr = root_vectors(value, c_root, &needed)) != MPI_SUCCESS) return;
    if (needed && (*ierr = make_vectors(value, sc, displs, &v)) != MPI_SUCCESS) return;
    *ierr = MPI_Scatterv_c(buffer(send), v ? v->counts : NULL, v ? v->displs : NULL,
                           datatype(st), buffer(recv), (MPI_Count)*rc, datatype(rt), c_root, value);
    free_vectors(v);
}
void bpack_mpi_reduce_scatter_(void *send, void *recv, const i8 *rc, const i8 *type,
                             const i8 *operation, const i8 *c, i8 *ierr)
{
    struct vectors *v;
    MPI_Comm value = comm(c);
    if ((*ierr = make_vectors(value, rc, NULL, &v)) != MPI_SUCCESS) return;
    *ierr = MPI_Reduce_scatter_c(buffer(send), buffer(recv), v->counts,
                                 datatype(type), op(operation), value);
    free_vectors(v);
}

#define NONBLOCKING_PT2PT(name, api) \
void bpack_mpi_##name##_(void *buf, const i8 *count, const i8 *type, const i8 *peer, \
                         const i8 *tag, const i8 *c, i8 *request, i8 *ierr) \
{ \
    INT_ARG(peer); \
    INT_ARG(tag); \
    MPI_Request value = MPI_REQUEST_NULL; \
    *ierr = api(buffer(buf), (MPI_Count)*count, datatype(type), c_peer, c_tag, comm(c), &value); \
    *request = MPI_Request_c2f(value); \
}
NONBLOCKING_PT2PT(isend, MPI_Isend_c)
NONBLOCKING_PT2PT(irecv, MPI_Irecv_c)

void bpack_mpi_recv_(void *buf, const i8 *count, const i8 *type, const i8 *peer,
                     const i8 *tag, const i8 *c, i8 *status, i8 *ierr)
{
    INT_ARG(peer);
    INT_ARG(tag);
    MPI_Status s;
    *ierr = MPI_Recv_c(buffer(buf), (MPI_Count)*count, datatype(type), c_peer, c_tag,
                       comm(c), ignore_status(status) ? MPI_STATUS_IGNORE : &s);
    if (*ierr == MPI_SUCCESS) status_out(&s, status);
}
void bpack_mpi_probe_(const i8 *peer, const i8 *tag, const i8 *c, i8 *status, i8 *ierr)
{
    INT_ARG(peer);
    INT_ARG(tag);
    MPI_Status s;
    *ierr = MPI_Probe(c_peer, c_tag, comm(c), ignore_status(status) ? MPI_STATUS_IGNORE : &s);
    if (*ierr == MPI_SUCCESS) status_out(&s, status);
}
void bpack_mpi_iprobe_(const i8 *peer, const i8 *tag, const i8 *c, i8 *flag, i8 *status, i8 *ierr)
{
    INT_ARG(peer);
    INT_ARG(tag);
    MPI_Status s;
    int value = 0;
    *ierr = MPI_Iprobe(c_peer, c_tag, comm(c), &value,
                       ignore_status(status) ? MPI_STATUS_IGNORE : &s);
    *flag = value != 0;  /* gfortran default LOGICAL(kind=8): false=0, true=1. */
    if (*ierr == MPI_SUCCESS && value) status_out(&s, status);
}
void bpack_mpi_get_count_(const i8 *status, const i8 *type, i8 *count, i8 *ierr)
{
    MPI_Status s;
    MPI_Count value = 0;
    status_in(status, &s);
    *ierr = MPI_Get_count_c(&s, datatype(type), &value);
    *count = (i8)value;
}
void bpack_mpi_wait_(i8 *request, i8 *status, i8 *ierr)
{
    MPI_Fint old = (MPI_Fint)*request;
    MPI_Request value = MPI_Request_f2c(old);
    MPI_Status s;
    *ierr = MPI_Wait(&value, ignore_status(status) ? MPI_STATUS_IGNORE : &s);
    *request = MPI_Request_c2f(value);
    if (value == MPI_REQUEST_NULL) release_vectors(old);
    if (*ierr == MPI_SUCCESS) status_out(&s, status);
}
static int requests_in(int n, const i8 *f, MPI_Request **out)
{
    *out = malloc((size_t)(n ? n : 1) * sizeof(**out));
    if (!*out) return MPI_ERR_NO_MEM;
    for (int j = 0; j < n; ++j) (*out)[j] = MPI_Request_f2c((MPI_Fint)f[j]);
    return MPI_SUCCESS;
}
static void requests_out(int n, const MPI_Request *values, i8 *f)
{
    for (int j = 0; j < n; ++j) {
        if (values[j] == MPI_REQUEST_NULL) release_vectors((MPI_Fint)f[j]);
        f[j] = MPI_Request_c2f(values[j]);
    }
}
void bpack_mpi_waitall_(const i8 *n, i8 *requests, i8 *statuses, i8 *ierr)
{
    INT_ARG(n);
    if (c_n < 0) { *ierr = MPI_ERR_COUNT; return; }
    MPI_Request *values;
    MPI_Status *s = NULL;
    if ((*ierr = requests_in(c_n, requests, &values)) != MPI_SUCCESS) return;
    if (!ignore_status(statuses)) {
        s = calloc((size_t)(c_n ? c_n : 1), sizeof(*s));
        if (!s) { free(values); *ierr = MPI_ERR_NO_MEM; return; }
    }
    *ierr = MPI_Waitall(c_n, values, s ? s : MPI_STATUSES_IGNORE);
    requests_out(c_n, values, requests);
    int error_class = MPI_SUCCESS;
    if (*ierr != MPI_SUCCESS) MPI_Error_class((int)*ierr, &error_class);
    if (s && (*ierr == MPI_SUCCESS || error_class == MPI_ERR_IN_STATUS))
        for (int j = 0; j < c_n; ++j) status_out(&s[j], statuses + j * bpack_mpi_status_size);
    free(s);
    free(values);
}
void bpack_mpi_waitany_(const i8 *n, i8 *requests, i8 *index, i8 *status, i8 *ierr)
{
    INT_ARG(n);
    if (c_n < 0) { *ierr = MPI_ERR_COUNT; return; }
    MPI_Request *values;
    MPI_Status s;
    int value = MPI_UNDEFINED;
    if ((*ierr = requests_in(c_n, requests, &values)) != MPI_SUCCESS) return;
    *ierr = MPI_Waitany(c_n, values, &value, ignore_status(status) ? MPI_STATUS_IGNORE : &s);
    requests_out(c_n, values, requests);
    *index = value == MPI_UNDEFINED ? MPI_UNDEFINED : (i8)value + 1;
    if (*ierr == MPI_SUCCESS) status_out(&s, status);
    free(values);
}
double bpack_mpi_wtime_(void) { return MPI_Wtime(); }
double bpack_mpi_wtick_(void) { return MPI_Wtick(); }
