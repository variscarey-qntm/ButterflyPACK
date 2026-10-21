! Boundary-only regression: no real distributed numerical routines are executed.
module ilp64_scalapack_fixture
   use iso_fortran_env, only: int64
   implicit none
   integer, parameter :: descriptor(9) = [1, 7, 3, 3, 2, 2, 0, 0, 3]
   integer, parameter :: untouched = -991
   integer(int64), parameter :: wide = shiftl(1_int64, 40) + 7_int64
   character(32) :: mode = ''
contains
   subroutine check(condition, label)
      logical, intent(in) :: condition
      character(*), intent(in) :: label
      if (.not.condition) then
         write(*,*) 'FAIL: ', label
         error stop 1
      end if
   end subroutine

   subroutine check_descriptor(desc)
      integer(int64), intent(in) :: desc(9)
      call check(all(desc == int(descriptor,int64)), '64-bit descriptor entries and stride')
   end subroutine

   subroutine descriptor_result(desc, m, n, mb, nb, irsrc, icsrc, ctxt, lld)
      integer(int64), intent(out) :: desc(9)
      integer(int64), intent(in) :: m, n, mb, nb, irsrc, icsrc, ctxt, lld
      call check(m == 3 .and. n == 3 .and. mb == 2 .and. nb == 2, 'descriptor dimension integer ABI')
      call check(irsrc == 0 .and. icsrc == 0 .and. ctxt == 7 .and. lld == 3, 'descriptor layout integer ABI')
      desc = int(descriptor,int64)
      if (mode == 'overflow-descriptor') desc(9) = wide
   end subroutine

   subroutine lu_result(m, n, ia, ja, desc, piv, info)
      integer(int64), intent(in) :: m, n, ia, ja, desc(9)
      integer(int64), intent(out) :: piv(*), info
      call check_descriptor(desc)
      call check(m == 3 .and. n == 3 .and. ia == 1 .and. ja == 1, 'LU scalar integer ABI')
      piv(1:3) = [2_int64, 3_int64, 3_int64]
      info = 0_int64
      if (mode == 'overflow-info') info = wide
      if (mode == 'overflow-pivot') piv(1) = wide
   end subroutine

   subroutine inverse_result(n, ia, ja, desc, piv, lwork, iw, liwork, info)
      integer(int64), intent(in) :: n, ia, ja, desc(9), piv(*), lwork, liwork
      integer(int64), intent(out) :: iw(*), info
      integer :: i
      call check_descriptor(desc)
      call check(n == 3 .and. ia == 1 .and. ja == 1, 'inverse scalar integer ABI')
      info = 0_int64
      if (mode == 'query-error') then
         info = -17_int64
         return
      end if
      if (lwork == -1 .or. liwork == -1) then
         iw(1) = 9_int64
         if (mode == 'overflow-iwork') iw(1) = wide
         return
      end if
      call check(lwork == 32 .and. liwork == 9, 'inverse workspace lengths')
      call check(all(piv(1:3) == [2_int64,3_int64,3_int64]), 'inverse input pivots')
      do i = 1, 9
         iw(i) = int(100+i,int64)
      end do
   end subroutine

   subroutine qr_result(m, n, ia, ja, desc, piv, query, perm, swaps, rank, info, truncate)
      integer(int64), intent(in) :: m, n, ia, ja, desc(9)
      integer(int64), intent(out) :: piv(*), swaps(*), rank, info
      integer(int64), intent(inout) :: perm(*)
      logical, intent(in) :: query, truncate
      call check_descriptor(desc)
      info = 0_int64
      if (query) return
      call check(all(perm(1:3) == [1_int64,2_int64,3_int64]), 'QR input permutation')
      if (truncate) then
         piv(1:3) = [1_int64,2_int64,3_int64]
         rank = 0_int64
      else if (ja == 2) then
         call check(m == 2 .and. n == 2 .and. ia == 2, 'QR submatrix offsets')
         piv(2:3) = [3_int64,2_int64]
         perm(2:3) = [3_int64,2_int64]
         swaps(2) = 3_int64
         rank = 1_int64
      else
         call check(m == 3 .and. n == 3 .and. ia == 1 .and. ja == 1, 'QR scalar integer ABI')
         piv(1:3) = [3_int64,2_int64,1_int64]
         perm(1:3) = [3_int64,2_int64,1_int64]
         swaps(1) = 3_int64
         rank = 1_int64
      end if
   end subroutine

   subroutine hessenberg_check(wantt, wantz, n, ilo, ihi, desca, iloz, ihiz, descz, lwork, ilwork)
      logical(kind=int64), intent(in) :: wantt, wantz
      integer(int64), intent(in) :: n, ilo, ihi, desca(9), iloz, ihiz, descz(9), lwork, ilwork
      call check_descriptor(desca)
      call check_descriptor(descz)
      call check(transfer(wantt,0_int64) == 1_int64, '64-bit WANTT representation')
      call check(transfer(wantz,0_int64) == 0_int64, '64-bit WANTZ representation')
      call check(n == 3 .and. ilo == 1 .and. ihi == 3, 'LAHQR integer ABI')
      call check(iloz == 1 .and. ihiz == 3 .and. ilwork == 1, 'LAHQR bounds and integer workspace')
      call check(lwork == -1 .or. lwork == 32, 'LAHQR workspace query sign extension')
   end subroutine

   subroutine lapiv_result(direc, rowcol, pivroc, m, n, ia, ja, desca, piv, ip, jp, descip, iw, last)
      character, intent(in) :: direc, rowcol, pivroc
      integer(int64), intent(in) :: m, n, ia, ja, desca(9), piv(*), ip, jp, descip(9)
      integer(int64), intent(out) :: iw(*), last
      integer :: i
      call check_descriptor(desca)
      call check(direc == 'B' .and. rowcol == pivroc, 'LAPIV transpose orientation')
      call check(m == 3 .and. n == 3 .and. ia == 1 .and. ja == 1, 'LAPIV scalar ABI')
      if (rowcol == 'C') then
         call check(ip == 3 .and. jp == 1, 'LAPIV original row offset')
         call check(all(descip == [1_int64,7_int64,5_int64,1_int64,2_int64,1_int64,0_int64,0_int64,5_int64]), &
            'LAPIV column-vector descriptor')
         last = piv(ip+n-1_int64)
      else
         call check(ip == 1 .and. jp == 3, 'LAPIV original column offset')
         call check(all(descip == [1_int64,7_int64,1_int64,5_int64,1_int64,2_int64,0_int64,0_int64,1_int64]), &
            'LAPIV row-vector descriptor')
         last = piv(jp+m-1_int64)
      end if
      call check(last == 5_int64, 'LAPIV final original-index pivot beyond the first block')
      call check(all(piv(1:5) == [1_int64,2_int64,3_int64,4_int64,5_int64]), 'LAPIV complete pivot prefix')
      do i = 1, 7
         iw(i) = int(200+i,int64)
      end do
   end subroutine
end module

program test_ilp64_scalapack
   use bpack_ilp64
   use ilp64_scalapack_fixture
   implicit none
   real(kind=4) :: sa(3,3)
   complex(kind=4) :: ca(3,3)
   real(kind=8) :: da(3,3), dw(32)
   complex(kind=8) :: za(3,3)
   integer :: piv(5), info, iw(9), empty(0), desc(9), grid(6), nlocal, ctxt, pmap(2,2)
   character :: topology
   call get_command_argument(1,mode)
   if (mode == 'overflow') mode = 'overflow-numroc'
   sa = 0
   da = 0
   ca = 0
   za = 0
   ! CTest runs these modes with WILL_FAIL: returning normally must fail the test.
   if (mode == 'overflow-info' .or. mode == 'overflow-pivot') then
      call pdgetrf(3,3,da,1,1,descriptor,piv,info)
      write(*,*) 'FAIL: large ILP64 output was not rejected'
      stop 0
   else if (mode == 'overflow-iwork') then
      call pdgetri(3,da,1,1,descriptor,empty,dw,-1,iw,-1,info)
      write(*,*) 'FAIL: large ILP64 workspace size was not rejected'
      stop 0
   else if (mode == 'overflow-descriptor') then
      call descinit(desc,3,3,2,2,0,0,7,3,info)
      write(*,*) 'FAIL: large ILP64 descriptor entry was not rejected'
      stop 0
   else if (mode == 'overflow-grid') then
      call blacs_gridinfo(7,grid(2),grid(3),grid(4),grid(5))
      write(*,*) 'FAIL: large ILP64 grid result was not rejected'
      stop 0
   else if (mode == 'overflow-numroc') then
      nlocal = numroc(3,2,0,0,1)
      write(*,*) 'FAIL: large ILP64 NUMROC result was not rejected', nlocal
      stop 0
   else if (mode == 'overflow-handle') then
      ctxt = sys2blacs_handle(1234567)
      write(*,*) 'FAIL: large ILP64 BLACS handle was not rejected', ctxt
      stop 0
   end if
   call check(mode == '', 'unknown test mode')

   grid = untouched
   call blacs_gridinfo(7,grid(2),grid(3),grid(4),grid(5))
   call check(all(grid == [untouched,1,1,0,0,untouched]), 'BLACS grid output copyback and guards')
   nlocal = numroc(3,2,0,0,1)
   call check(nlocal == 3, 'NUMROC function result copyback')
   ctxt = sys2blacs_handle(1234567)
   call check(ctxt == -1900000001, 'SYS2BLACS input communicator and signed result')
   pmap = reshape([10,11,12,13],[2,2])
   ctxt = 7
   call blacs_gridmap(ctxt,pmap,2,2,2)
   call check(ctxt == 37 .and. all(pmap == reshape([10,11,12,13],[2,2])), 'BLACS map and context copyback')
   call blacs_gridexit(ctxt)
   call blacs_exit(1)
   call pb_topget(7,'Broadcast','Rowwise',topology)
   call check(topology == 'R', 'PB_TOPGET topology output')
   call pb_topset(7,'Broadcast','Rowwise',topology)
   desc = untouched
   call descinit(desc,3,3,2,2,0,0,7,3,info)
   call check(info == 0 .and. all(desc == descriptor), 'DESCINIT descriptor and INFO copyback')
   mode = 'descriptor-error'
   call descinit(desc,3,3,2,2,0,0,7,3,info)
   call check(info == -17 .and. all(desc == descriptor), 'DESCINIT negative INFO sign extension')
   mode = ''
   desc = untouched
   call descset(desc,3,3,2,2,0,0,7,3)
   call check(all(desc == descriptor), 'DESCSET descriptor output copyback')

   piv = untouched
   call psgetrf(3,3,sa,1,1,descriptor,piv,info)
   call check(info == 0 .and. all(piv == [2,3,3,untouched,untouched]), 'single real LU')
   piv = untouched
   call pcgetrf(3,3,ca,1,1,descriptor,piv,info)
   call check(info == 0 .and. all(piv == [2,3,3,untouched,untouched]), 'single complex LU')
   piv = untouched
   call pdgetrf(3,3,da,1,1,descriptor,piv,info)
   call check(info == 0 .and. all(piv == [2,3,3,untouched,untouched]), 'double real LU')
   piv = untouched
   call pzgetrf(3,3,za,1,1,descriptor,piv,info)
   call check(info == 0 .and. all(piv == [2,3,3,untouched,untouched]), 'double complex LU')
   call pdgetrfmod(3,3,da,1,1,descriptor,0.125_8,piv,info)
   call check(info == 0 .and. all(piv == [2,3,3,untouched,untouched]), 'modified real LU')
   call pzgetrfmod(3,3,za,1,1,descriptor,0.125_8,piv,info)
   call check(info == 0 .and. all(piv == [2,3,3,untouched,untouched]), 'modified complex LU')
   call test_real()
   call test_complex()
   call test_lapiv()
   write(*,*) 'PASS: mocked distributed ILP64 integer, descriptor, pivot, workspace and logical adapters'
contains
   subroutine test_real()
      real(kind=8) :: a(3,3), tau(3), work(32), rw(3), wi(3), z(3,3)
      integer :: perm(3), swaps(3), rank, i
      a = 0
      tau = 0
      work = 0
      z = 0
      iw = untouched
      call pdgetri(3,a,1,1,descriptor,empty,work,-1,iw,-1,info)
      call check(info == 0 .and. work(1) == 24 .and. iw(1) == 9, 'real inverse query sizes')
      call check(all(iw(2:9) == untouched), 'real query copies only defined IWORK(1)')
      mode = 'query-error'
      iw = untouched
      call pdgetri(3,a,1,1,descriptor,empty,work,-1,iw,-1,info)
      call check(info == -17 .and. all(iw == untouched), 'negative 64-bit query INFO')
      mode = ''
      call pdgetri(3,a,1,1,descriptor,piv,work,32,iw,9,info)
      call check(info == 0 .and. all(iw == [(100+i,i=1,9)]), 'real integer workspace copyback')

      rank = untouched
      call pdgeqpfmod(3,3,a,1,1,descriptor,empty,tau,work,-1,info,empty,empty,rank,0.0_8,0.0_8)
      call check(info == 0 .and. rank == untouched .and. work(1) == 24, 'real QR empty query pivots')
      piv = untouched
      perm = [1,2,3]
      swaps = untouched
      call pdgeqpfmod(3,3,a,1,1,descriptor,piv,tau,work,32,info,perm,swaps,rank,0.0_8,0.0_8)
      call check(info == 0 .and. rank == 1, 'real QR rank output')
      call check(all(piv == [3,2,1,untouched,untouched]), 'real QR local pivots')
      call check(all(perm == [3,2,1]) .and. all(swaps == [3,untouched,untouched]), 'real QR permutations')
      perm = [1,2,3]
      swaps = untouched
      call pdgeqpfmod(3,3,a,1,1,descriptor,piv,tau,work,32,info,perm,swaps,rank,0.0_8,1.0_8)
      call check(rank == 0 .and. all(swaps == untouched), 'rank-zero QR does not copy undefined swaps')
      piv = untouched
      perm = [1,2,3]
      call pdgeqpfmod(2,2,a,2,2,descriptor,piv,tau,work,32,info,perm,swaps,rank,0.0_8,0.0_8)
      call check(all(piv == [untouched,3,2,untouched,untouched]), 'QR preserves pivot prefix and tail')
      call check(all(swaps == [untouched,3,untouched]), 'QR submatrix global swap indices')

      iw = untouched
      call pdlahqr(.true.,.false.,3,1,3,a,descriptor,rw,wi,1,3,z,descriptor,work,-1,iw,1,info)
      call check(info == 0 .and. all(iw == untouched), 'real LAHQR undefined query IWORK')
      call pdlahqr(.true.,.false.,3,1,3,a,descriptor,rw,wi,1,3,z,descriptor,work,32,iw,1,info)
      call check(info == 0 .and. all(rw == 3) .and. all(wi == 0), 'real LAHQR output')
   end subroutine

   subroutine test_complex()
      complex(kind=8) :: a(3,3), tau(3), work(32), w(3), z(3,3), vl(3,3), vr(3,3)
      real(kind=8) :: rwork(32)
      integer :: perm(3), swaps(3), rank, m, i
      logical :: select(3)
      a = 0
      tau = 0
      work = 0
      rwork = 0
      z = 0
      vl = 0
      vr = 0
      piv = [2,3,3,untouched,untouched]
      iw = untouched
      call pzgetri(3,a,1,1,descriptor,empty,work,-1,iw,-1,info)
      call check(info == 0 .and. real(work(1)) == 24 .and. iw(1) == 9, 'complex inverse query')
      call check(all(iw(2:9) == untouched), 'complex query copies only defined IWORK(1)')
      call pzgetri(3,a,1,1,descriptor,piv,work,32,iw,9,info)
      call check(info == 0 .and. all(iw == [(100+i,i=1,9)]), 'complex integer workspace copyback')

      rank = untouched
      call pzgeqpfmod(3,3,a,1,1,descriptor,empty,tau,work,-1,rwork,-1,info,empty,empty,rank,0.0_8,0.0_8)
      call check(info == 0 .and. rank == untouched, 'complex QR empty query pivots')
      call check(real(work(1)) == 24 .and. rwork(1) == 12, 'complex QR workspace sizes')
      call pzgeqpfmod(3,3,a,1,1,descriptor,empty,tau,work,32,rwork,-1,info,empty,empty,rank,0.0_8,0.0_8)
      call check(rank == untouched, 'RWORK-only query does not copy undefined rank')
      piv = untouched
      perm = [1,2,3]
      swaps = untouched
      call pzgeqpfmod(3,3,a,1,1,descriptor,piv,tau,work,32,rwork,32,info,perm,swaps,rank,0.0_8,0.0_8)
      call check(info == 0 .and. rank == 1, 'complex QR rank')
      call check(all(piv == [3,2,1,untouched,untouched]), 'complex QR pivots')
      call check(all(perm == [3,2,1]) .and. all(swaps == [3,untouched,untouched]), 'complex QR swaps')
      perm = [1,2,3]
      swaps = untouched
      call pzgeqpfmod(3,3,a,1,1,descriptor,piv,tau,work,32,rwork,32,info,perm,swaps,rank,0.0_8,1.0_8)
      call check(rank == 0 .and. all(swaps == untouched), 'complex rank-zero swaps')

      iw = untouched
      call pzlahqr(.true.,.false.,3,1,3,a,descriptor,w,1,3,z,descriptor,work,-1,iw,1,info)
      call check(info == 0 .and. all(iw == untouched), 'complex LAHQR undefined query IWORK')
      call pzlahqr(.true.,.false.,3,1,3,a,descriptor,w,1,3,z,descriptor,work,32,iw,1,info)
      call check(info == 0 .and. all(abs(w-3) == 0), 'complex LAHQR output')
      select = [.true.,.false.,.true.]
      m = untouched
      call pztrevc('R','S',select,3,a,descriptor,vl,descriptor,vr,descriptor,3,m,work,rwork,info)
      call check(info == 0 .and. m == 2, 'TREVC 64-bit SELECT and M')
   end subroutine

   subroutine test_lapiv()
      integer :: vector_desc(9), input(5), work(9), ip, jp, orientation, i
      character :: axis
      input = [1,2,3,4,5]
      do orientation = 1, 2
         if (orientation == 1) then
            axis = 'C'
            ip = 3
            jp = 1
            vector_desc = [1,7,5,1,2,1,0,0,5]
         else
            axis = 'R'
            ip = 1
            jp = 3
            vector_desc = [1,7,1,5,1,2,0,0,1]
         end if
         work = untouched
         call pslapiv('B',axis,axis,3,3,sa,1,1,descriptor,input,ip,jp,vector_desc,work)
         call check(sa(1,1) == 5 .and. all(work(1:7) == [(200+i,i=1,7)]), 'single real LAPIV prefix')
         call check(all(work(8:9) == untouched), 'single real LAPIV workspace guards')
         work = untouched
         call pdlapiv('B',axis,axis,3,3,da,1,1,descriptor,input,ip,jp,vector_desc,work)
         call check(da(1,1) == 5 .and. all(work(1:7) == [(200+i,i=1,7)]), 'double real LAPIV prefix')
         call check(all(work(8:9) == untouched), 'double real LAPIV workspace guards')
         work = untouched
         call pclapiv('B',axis,axis,3,3,ca,1,1,descriptor,input,ip,jp,vector_desc,work)
         call check(ca(1,1) == 5 .and. all(work(1:7) == [(200+i,i=1,7)]), 'single complex LAPIV prefix')
         call check(all(work(8:9) == untouched), 'single complex LAPIV workspace guards')
         work = untouched
         call pzlapiv('B',axis,axis,3,3,za,1,1,descriptor,input,ip,jp,vector_desc,work)
         call check(za(1,1) == 5 .and. all(work(1:7) == [(200+i,i=1,7)]), 'double complex LAPIV prefix')
         call check(all(work(8:9) == untouched), 'double complex LAPIV workspace guards')
         call check(all(input == [1,2,3,4,5]), 'LAPIV input pivots are preserved')
      end do
   end subroutine
end program

! These mocks deliberately have ILP64 signatures, independently of the adapters.
subroutine blacs_gridinfo(ctxt, nr, nc, ir, ic)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(in) :: ctxt
   integer(int64), intent(out) :: nr, nc, ir, ic
   call check(ctxt == 7_int64, 'BLACS context integer ABI')
   nr = 1_int64
   nc = 1_int64
   ir = 0_int64
   ic = 0_int64
   if (mode == 'overflow-grid') nr = wide
end subroutine

integer(kind=8) function numroc(n, nb, iproc, isrcproc, nprocs)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(in) :: n, nb, iproc, isrcproc, nprocs
   call check(n >= 0 .and. nb == 2 .and. iproc == 0 .and. isrcproc == 0 .and. nprocs == 1, &
      'NUMROC integer ABI')
   numroc = n
   if (mode == 'overflow-numroc') numroc = wide
end function

subroutine blacs_gridmap(ctxt, pmap, ldu, nr, nc)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(inout) :: ctxt
   integer(int64), intent(in) :: ldu, nr, nc, pmap(ldu,*)
   call check(ctxt == 7 .and. ldu == 2 .and. nr == 2 .and. nc == 2, 'BLACS_GRIDMAP scalar ABI')
   call check(all(pmap(1:2,1:2) == reshape([10_int64,11_int64,12_int64,13_int64],[2,2])), &
      'BLACS_GRIDMAP 64-bit map entries and stride')
   ctxt = 37_int64
end subroutine

subroutine blacs_gridexit(ctxt)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(in) :: ctxt
   call check(ctxt == 37_int64, 'BLACS_GRIDEXIT context ABI')
end subroutine

subroutine blacs_exit(flag)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(in) :: flag
   call check(flag == 1_int64, 'BLACS_EXIT flag ABI')
end subroutine

subroutine pb_topget(ctxt, operation, scope, topology)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(in) :: ctxt
   character(*), intent(in) :: operation, scope
   character(*), intent(out) :: topology
   call check(ctxt == 7 .and. operation == 'Broadcast' .and. scope == 'Rowwise', 'PB_TOPGET input ABI')
   topology = 'R'
end subroutine

subroutine pb_topset(ctxt, operation, scope, topology)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(in) :: ctxt
   character(*), intent(in) :: operation, scope, topology
   call check(ctxt == 7 .and. operation == 'Broadcast' .and. scope == 'Rowwise', 'PB_TOPSET input ABI')
   call check(topology == 'R', 'PB_TOPSET topology input')
end subroutine

integer(kind=8) function sys2blacs_handle(comm)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(in) :: comm
   call check(comm == 1234567_int64, 'SYS2BLACS communicator integer ABI')
   sys2blacs_handle = -1900000001_int64
   if (mode == 'overflow-handle') sys2blacs_handle = wide
end function

subroutine descinit(desc, m, n, mb, nb, irsrc, icsrc, ctxt, lld, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(out) :: desc(9), info
   integer(int64), intent(in) :: m, n, mb, nb, irsrc, icsrc, ctxt, lld
   call descriptor_result(desc,m,n,mb,nb,irsrc,icsrc,ctxt,lld)
   info = 0_int64
   if (mode == 'descriptor-error') info = -17_int64
end subroutine

subroutine descset(desc, m, n, mb, nb, irsrc, icsrc, ctxt, lld)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64), intent(out) :: desc(9)
   integer(int64), intent(in) :: m, n, mb, nb, irsrc, icsrc, ctxt, lld
   call descriptor_result(desc,m,n,mb,nb,irsrc,icsrc,ctxt,lld)
end subroutine

subroutine psgetrf(m, n, a, ia, ja, desc, piv, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), info
   real(kind=4) :: a(desc(9),*)
   call lu_result(m,n,ia,ja,desc,piv,info)
   a(1,1) = 2
end subroutine

subroutine pdgetrf(m, n, a, ia, ja, desc, piv, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), info
   real(kind=8) :: a(desc(9),*)
   call lu_result(m,n,ia,ja,desc,piv,info)
   a(1,1) = 2
end subroutine

subroutine pcgetrf(m, n, a, ia, ja, desc, piv, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), info
   complex(kind=4) :: a(desc(9),*)
   call lu_result(m,n,ia,ja,desc,piv,info)
   a(1,1) = 2
end subroutine

subroutine pzgetrf(m, n, a, ia, ja, desc, piv, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), info
   complex(kind=8) :: a(desc(9),*)
   call lu_result(m,n,ia,ja,desc,piv,info)
   a(1,1) = 2
end subroutine

subroutine pdgetrfmod(m, n, a, ia, ja, desc, thresh, piv, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), info
   real(kind=8) :: a(desc(9),*), thresh
   call check(thresh == 0.125_8, 'real LU threshold')
   call lu_result(m,n,ia,ja,desc,piv,info)
   a(1,1) = 2
end subroutine

subroutine pzgetrfmod(m, n, a, ia, ja, desc, thresh, piv, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), info
   complex(kind=8) :: a(desc(9),*)
   real(kind=8) :: thresh
   call check(thresh == 0.125_8, 'complex LU threshold')
   call lu_result(m,n,ia,ja,desc,piv,info)
   a(1,1) = 2
end subroutine

subroutine pdgetri(n, a, ia, ja, desc, piv, work, lwork, iw, liwork, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: n, ia, ja, desc(9), piv(*), lwork, iw(*), liwork, info
   real(kind=8) :: a(desc(9),*), work(*)
   call inverse_result(n,ia,ja,desc,piv,lwork,iw,liwork,info)
   work(1) = 24
   if (lwork /= -1 .and. liwork /= -1) a(1,1) = 0.5_8
end subroutine

subroutine pzgetri(n, a, ia, ja, desc, piv, work, lwork, iw, liwork, info)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: n, ia, ja, desc(9), piv(*), lwork, iw(*), liwork, info
   complex(kind=8) :: a(desc(9),*), work(*)
   call inverse_result(n,ia,ja,desc,piv,lwork,iw,liwork,info)
   work(1) = 24
   if (lwork /= -1 .and. liwork /= -1) a(1,1) = 0.5_8
end subroutine

subroutine pdgeqpfmod(m, n, a, ia, ja, desc, piv, tau, work, lwork, info, perm, swaps, rank, rtol, atol)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), lwork, info, perm(*), swaps(*), rank
   real(kind=8) :: a(desc(9),*), tau(*), work(*), rtol, atol
   call check(rtol == 0, 'real QR tolerance')
   work(1) = 24
   call qr_result(m,n,ia,ja,desc,piv,lwork == -1,perm,swaps,rank,info,atol > 0)
   if (lwork /= -1) then
      a(1,1) = 2
      tau(1) = 1
   end if
end subroutine

subroutine pzgeqpfmod(m, n, a, ia, ja, desc, piv, tau, work, lwork, rwork, lrwork, &
                     info, perm, swaps, rank, rtol, atol)
   use ilp64_scalapack_fixture
   implicit none
   integer(int64) :: m, n, ia, ja, desc(9), piv(*), lwork, lrwork, info, perm(*), swaps(*), rank
   complex(kind=8) :: a(desc(9),*), tau(*), work(*)
   real(kind=8) :: rwork(*), rtol, atol
   call check(rtol == 0, 'complex QR tolerance')
   work(1) = 24
   rwork(1) = 12
   call qr_result(m,n,ia,ja,desc,piv,lwork == -1 .or. lrwork == -1,perm,swaps,rank,info,atol > 0)
   if (lwork /= -1 .and. lrwork /= -1) then
      a(1,1) = 2
      tau(1) = 1
   end if
end subroutine

subroutine pdlahqr(wantt, wantz, n, ilo, ihi, a, desca, wr, wi, iloz, ihiz, z, descz, &
                  work, lwork, iw, ilwork, info)
   use ilp64_scalapack_fixture
   implicit none
   logical(kind=int64) :: wantt, wantz
   integer(int64) :: n, ilo, ihi, desca(9), iloz, ihiz, descz(9), lwork, iw(*), ilwork, info
   real(kind=8) :: a(desca(9),*), wr(*), wi(*), z(descz(9),*), work(*)
   call hessenberg_check(wantt,wantz,n,ilo,ihi,desca,iloz,ihiz,descz,lwork,ilwork)
   info = 0_int64
   work(1) = 24
   if (lwork == -1) return
   wr(1:3) = 3
   wi(1:3) = 0
   a(1,1) = 3
   z(1,1) = 1
   iw(1) = 0_int64
end subroutine

subroutine pzlahqr(wantt, wantz, n, ilo, ihi, a, desca, w, iloz, ihiz, z, descz, &
                  work, lwork, iw, ilwork, info)
   use ilp64_scalapack_fixture
   implicit none
   logical(kind=int64) :: wantt, wantz
   integer(int64) :: n, ilo, ihi, desca(9), iloz, ihiz, descz(9), lwork, iw(*), ilwork, info
   complex(kind=8) :: a(desca(9),*), w(*), z(descz(9),*), work(*)
   call hessenberg_check(wantt,wantz,n,ilo,ihi,desca,iloz,ihiz,descz,lwork,ilwork)
   info = 0_int64
   work(1) = 24
   if (lwork == -1) return
   w(1:3) = 3
   a(1,1) = 3
   z(1,1) = 1
   iw(1) = 0_int64
end subroutine

subroutine pztrevc(side, howmny, select, n, t, desct, vl, descvl, vr, descvr, mm, m, work, rwork, info)
   use ilp64_scalapack_fixture
   implicit none
   character :: side, howmny
   logical(kind=int64) :: select(*)
   integer(int64) :: n, desct(9), descvl(9), descvr(9), mm, m, info
   complex(kind=8) :: t(desct(9),*), vl(descvl(9),*), vr(descvr(9),*), work(*)
   real(kind=8) :: rwork(*)
   call check_descriptor(desct)
   call check_descriptor(descvl)
   call check_descriptor(descvr)
   call check(side == 'R' .and. howmny == 'S' .and. n == 3 .and. mm == 3, 'TREVC scalar ABI')
   call check(all(transfer(select(1:3),[0_int64],3) == [1_int64,0_int64,1_int64]), '64-bit SELECT stride')
   m = 2_int64
   info = 0_int64
   t(1,1) = 3
   vl(1,1) = 1
   vr(1,1) = 1
   work(1) = 0
   rwork(1) = 0
end subroutine

subroutine pslapiv(direc, rowcol, pivroc, m, n, a, ia, ja, desca, piv, ip, jp, descip, iw)
   use ilp64_scalapack_fixture
   implicit none
   character :: direc, rowcol, pivroc
   integer(int64) :: m, n, ia, ja, desca(9), piv(*), ip, jp, descip(9), iw(*), last
   real(kind=4) :: a(desca(9),*)
   call lapiv_result(direc,rowcol,pivroc,m,n,ia,ja,desca,piv,ip,jp,descip,iw,last)
   a(1,1) = real(last,kind=4)
end subroutine

subroutine pdlapiv(direc, rowcol, pivroc, m, n, a, ia, ja, desca, piv, ip, jp, descip, iw)
   use ilp64_scalapack_fixture
   implicit none
   character :: direc, rowcol, pivroc
   integer(int64) :: m, n, ia, ja, desca(9), piv(*), ip, jp, descip(9), iw(*), last
   real(kind=8) :: a(desca(9),*)
   call lapiv_result(direc,rowcol,pivroc,m,n,ia,ja,desca,piv,ip,jp,descip,iw,last)
   a(1,1) = real(last,kind=8)
end subroutine

subroutine pclapiv(direc, rowcol, pivroc, m, n, a, ia, ja, desca, piv, ip, jp, descip, iw)
   use ilp64_scalapack_fixture
   implicit none
   character :: direc, rowcol, pivroc
   integer(int64) :: m, n, ia, ja, desca(9), piv(*), ip, jp, descip(9), iw(*), last
   complex(kind=4) :: a(desca(9),*)
   call lapiv_result(direc,rowcol,pivroc,m,n,ia,ja,desca,piv,ip,jp,descip,iw,last)
   a(1,1) = real(last,kind=4)
end subroutine

subroutine pzlapiv(direc, rowcol, pivroc, m, n, a, ia, ja, desca, piv, ip, jp, descip, iw)
   use ilp64_scalapack_fixture
   implicit none
   character :: direc, rowcol, pivroc
   integer(int64) :: m, n, ia, ja, desca(9), piv(*), ip, jp, descip(9), iw(*), last
   complex(kind=8) :: a(desca(9),*)
   call lapiv_result(direc,rowcol,pivroc,m,n,ia,ja,desca,piv,ip,jp,descip,iw,last)
   a(1,1) = real(last,kind=8)
end subroutine
