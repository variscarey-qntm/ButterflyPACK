#include "bpack_mpi4.fi"
program test_mpi4
    use bpack_mpi4_constants
    use iso_c_binding, only: c_int64_t
    implicit none
    interface
        function zero_type() bind(C, name="bpack_mpi_test_zero_type") result(handle)
            import c_int64_t
            integer(c_int64_t) :: handle
        end function
        subroutine free_type(handle) bind(C, name="bpack_mpi_test_free_type")
            import c_int64_t
            integer(c_int64_t) :: handle
        end subroutine
        subroutine errors_return() bind(C, name="bpack_mpi_test_errors_return")
        end subroutine
        function thread_checks() bind(C, name="bpack_mpi_test_threads") result(success)
            import c_int64_t
            integer(c_int64_t) :: success
        end function
    end interface
    integer :: ierr, provided, rank, nproc, peer, parent, c, g, g2, newrank, idx, count, ztype
    integer :: requests(2), status(MPI_STATUS_SIZE), statuses(MPI_STATUS_SIZE, 2)
    integer :: ranks(2), counts(2), displs(2), x, y, send(2), recv(2), large
    logical :: flag
    double precision :: start

    provided = MPI_THREAD_SINGLE
    if (command_argument_count() == 0) then
        call MPI_Init_thread(MPI_THREAD_MULTIPLE, provided, ierr)
        call ok(ierr == MPI_SUCCESS, 'init_thread')
    else
        call MPI_Init(ierr)
        call ok(ierr == MPI_SUCCESS, 'init')
    end if
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call ok(ierr == MPI_SUCCESS, 'comm_rank')
    call MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
    call ok(ierr == MPI_SUCCESS .and. nproc == 2, 'requires two ranks')
    peer = 1 - rank
    start = MPI_Wtime()
    call ok(MPI_Wtick() > 0d0, 'wtick')
    call MPI_Comm_get_parent(parent, ierr)
    call ok(ierr == MPI_SUCCESS .and. parent == MPI_COMM_NULL, 'get_parent')
    call MPI_Comm_group(MPI_COMM_WORLD, g, ierr)
    call ok(ierr == MPI_SUCCESS, 'comm_group')
    ranks = [1, 0]
    call MPI_Group_incl(g, 2, ranks, g2, ierr)
    call ok(ierr == MPI_SUCCESS, 'group_incl')
    call MPI_Comm_create(MPI_COMM_WORLD, g2, c, ierr)
    call ok(ierr == MPI_SUCCESS, 'comm_create')
    call MPI_Comm_rank(c, newrank, ierr)
    call ok(ierr == MPI_SUCCESS .and. newrank == peer, 'reordered rank')
    call MPI_Comm_free(c, ierr)
    call ok(ierr == MPI_SUCCESS .and. c == MPI_COMM_NULL, 'comm_free')
    call MPI_Group_free(g2, ierr)
    call ok(ierr == MPI_SUCCESS .and. g2 == MPI_GROUP_NULL, 'group_free')
    call MPI_Group_free(g, ierr)
    call MPI_Comm_split(MPI_COMM_WORLD, 0, rank, c, ierr)
    call ok(ierr == MPI_SUCCESS, 'comm_split')
    call MPI_Comm_disconnect(c, ierr)
    call ok(ierr == MPI_SUCCESS .and. c == MPI_COMM_NULL, 'comm_disconnect')

    large = 2_8**33 + 17
    x = large + rank
    call MPI_Bcast(x, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. x == large, 'integer8 bcast')
    x = large + rank
    call MPI_Allreduce(x, y, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. y == 2*large + 1, 'integer8 allreduce')
    call MPI_Allreduce(MPI_IN_PLACE, x, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. x == y, 'in_place')
    x = large + rank
    call MPI_Reduce(x, y, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS, 'reduce')
    if (rank == 0) call ok(y == 2*large + 1, 'reduce payload')

    call MPI_Allgather(x, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. all(recv == [large, large+1]), 'allgather')
    counts = 1
    displs = [0, 1]
    call MPI_Allgatherv(x, 1, MPI_INTEGER, recv, counts, displs, MPI_INTEGER, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. all(recv == [large, large+1]), 'allgatherv')
    send = [100*rank, 100*rank+1]
    call MPI_Alltoall(send, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. all(recv == [rank, 100+rank]), 'alltoall')
    call MPI_Gather(x, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS, 'gather')
    if (rank == 0) call ok(all(recv == [large, large+1]), 'gather payload')
    call MPI_Gatherv(x, 1, MPI_INTEGER, recv, counts, displs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS, 'gatherv')
    if (rank == 0) call ok(all(recv == [large, large+1]), 'gatherv payload')
    send = [large, large+1]
    call MPI_Scatterv(send, counts, displs, MPI_INTEGER, y, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. y == large+rank, 'scatterv')
    send = [rank+1, rank+2]
    call MPI_Reduce_scatter(send, y, counts, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. y == 3+2*rank, 'reduce_scatter')

    call MPI_Iallgather(x, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, MPI_COMM_WORLD, requests(1), ierr)
    call ok(ierr == MPI_SUCCESS, 'iallgather')
    call MPI_Wait(requests(1), MPI_STATUS_IGNORE, ierr)
    call ok(ierr == MPI_SUCCESS .and. all(recv == [large, large+1]), 'wait ignored')
    call MPI_Iallgatherv(x, 1, MPI_INTEGER, recv, counts, displs, MPI_INTEGER, MPI_COMM_WORLD, requests(1), ierr)
    call ok(ierr == MPI_SUCCESS, 'iallgatherv')
    call MPI_Wait(requests(1), status, ierr)
    call ok(ierr == MPI_SUCCESS .and. all(recv == [large, large+1]), 'wait')
    send = [100*rank, 100*rank+1]
    call MPI_Ialltoall(send, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, MPI_COMM_WORLD, requests(1), ierr)
    call ok(ierr == MPI_SUCCESS, 'ialltoall')
    call MPI_Wait(requests(1), status, ierr)
    call ok(ierr == MPI_SUCCESS .and. all(recv == [rank, 100+rank]), 'ialltoall payload')

    call MPI_Irecv(y, 1, MPI_INTEGER, peer, 41, MPI_COMM_WORLD, requests(1), ierr)
    call ok(ierr == MPI_SUCCESS, 'irecv')
    call MPI_Isend(x, 1, MPI_INTEGER, peer, 41, MPI_COMM_WORLD, requests(2), ierr)
    call ok(ierr == MPI_SUCCESS, 'isend')
    call MPI_Waitany(2, requests, idx, status, ierr)
    call ok(ierr == MPI_SUCCESS .and. idx >= 1 .and. idx <= 2, 'waitany one-based index')
    call MPI_Waitall(2, requests, statuses, ierr)
    call ok(ierr == MPI_SUCCESS .and. y == large+peer, 'waitall')
    call ok(all(requests == MPI_REQUEST_NULL), 'request null conversion')
    call MPI_Waitany(2, requests, idx, MPI_STATUS_IGNORE, ierr)
    call ok(ierr == MPI_SUCCESS .and. idx == MPI_UNDEFINED, 'waitany undefined')

    call MPI_Isend(x, 1, MPI_INTEGER, peer, 42, MPI_COMM_WORLD, requests(1), ierr)
    call ok(ierr == MPI_SUCCESS, 'probe send')
    flag = .false.
    do while (.not. flag)
        call MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, flag, status, ierr)
        call ok(ierr == MPI_SUCCESS, 'iprobe')
    end do
    call ok(transfer(flag, x) == 1, 'logical flag representation')
    call ok(status(MPI_SOURCE) == peer .and. status(MPI_TAG) == 42, 'iprobe status')
    call MPI_Probe(peer, 42, MPI_COMM_WORLD, status, ierr)
    call ok(ierr == MPI_SUCCESS, 'probe')
    call MPI_Get_count(status, MPI_INTEGER, count, ierr)
    call ok(ierr == MPI_SUCCESS .and. count == 1, 'get_count')
    call MPI_Recv(y, 1, MPI_INTEGER, peer, 42, MPI_COMM_WORLD, status, ierr)
    call ok(ierr == MPI_SUCCESS .and. y == large+peer, 'recv')
    call ok(status(MPI_SOURCE) == peer .and. status(MPI_TAG) == 42, 'recv status')
    requests(2) = MPI_REQUEST_NULL
    call MPI_Waitall(2, requests, MPI_STATUSES_IGNORE, ierr)
    call ok(ierr == MPI_SUCCESS, 'waitall ignored')
    call MPI_Waitall(0, requests, MPI_STATUSES_IGNORE, ierr)
    call ok(ierr == MPI_SUCCESS, 'empty waitall')
    call errors_return()
    call MPI_Irecv(y, 0, MPI_INTEGER, peer, 43, MPI_COMM_WORLD, requests(1), ierr)
    call MPI_Isend(x, 1, MPI_INTEGER, peer, 43, MPI_COMM_WORLD, requests(2), ierr)
    call MPI_Waitall(2, requests, statuses, ierr)
    call ok(ierr /= MPI_SUCCESS, 'waitall error')
    call ok(statuses(MPI_ERROR, 1) /= MPI_SUCCESS, 'waitall per-request error')
    call ok(statuses(MPI_SOURCE, 1) == peer .and. statuses(MPI_TAG, 1) == 43, 'waitall error status')
    call MPI_Waitall(2, requests, MPI_STATUSES_IGNORE, ierr)
    call ok(ierr == MPI_SUCCESS, 'finish any pending send after waitall error')

    ! A zero-extent datatype tests large counts/displacements without large allocation.
    ztype = zero_type()
    counts = 2_8**31 + 3
    displs = [0_8, 2_8**33]
    call MPI_Bcast(MPI_BOTTOM, counts(1), ztype, 0, MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS, 'large-count bcast with bottom')
    call MPI_Iallgatherv(x, counts(1), ztype, recv, counts, displs, ztype, &
                        MPI_COMM_WORLD, requests(1), ierr)
    call ok(ierr == MPI_SUCCESS, 'large-count iallgatherv')
    call MPI_Iallgatherv(x, counts(1), ztype, recv, counts, displs, ztype, &
                        MPI_COMM_WORLD, requests(2), ierr)
    call ok(ierr == MPI_SUCCESS, 'second outstanding iallgatherv')
    call MPI_Waitall(2, requests, MPI_STATUSES_IGNORE, ierr)
    call ok(ierr == MPI_SUCCESS, 'large-count waitall')
    call free_type(ztype)
    if (provided == MPI_THREAD_MULTIPLE) call ok(thread_checks() == 1, 'concurrent vector requests and waits')
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    call ok(ierr == MPI_SUCCESS .and. MPI_Wtime() >= start, 'barrier and wtime')
    call MPI_Finalize(ierr)
    call ok(ierr == MPI_SUCCESS, 'finalize')
    if (rank == 0) print *, 'MPI4 bridge: all checks passed'
contains
    subroutine ok(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label
        if (.not. condition) then
            print *, 'MPI4 bridge failure: ', label
            error stop 1
        end if
    end subroutine ok
end program test_mpi4
