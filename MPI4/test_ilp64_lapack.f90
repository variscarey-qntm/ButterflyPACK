program test_ilp64_lapack
   use bpack_ilp64
   implicit none
   integer :: checks

   checks = 0
   call test_s()
   call test_d()
   call test_c()
   call test_z()
   print *, 'ILP64 LAPACK adapter checks passed:', checks

contains

   subroutine require(condition, label)
      logical, intent(in) :: condition
      character(*), intent(in) :: label
      if (.not. condition) then
         print *, 'FAILED: ', label
         error stop 1
      endif
      checks = checks + 1
   end subroutine

   subroutine test_s()
      real(kind=4), parameter :: one = 1.0_4, zero = 0.0_4, tol = 1.0e-4_4
      real(kind=4) :: a(2,2), original(2,2), b(2,2), u(2,2), vt(2,2)
      real(kind=4) :: work(1024), query(1), tau(2), singular(2), wr(2), wi(2), rwork(1024)
      integer :: piv(2), info, rank, iwork(16), m
      logical :: selected(2)

      original = zero
      original(1,1) = 2
      original(2,2) = 3
      a = original
      call sgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'sgetrf pivots')
      b = original
      call sgetrs('N',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'sgetrs')
      call sgetri(2,a,2,piv,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'sgetri query')
      call sgetri(2,a,2,piv,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-0.5_4) < tol, 'sgetri')
      a = original
      call sgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'sgetrfmod pivots')
      a = reshape([zero,2.0_4,one,3.0_4], [2,2])
      call sgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'sgetrf row interchange')
      a = reshape([zero,2.0_4,one,3.0_4], [2,2])
      call sgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'sgetrfmod row interchange')
      a = zero
      call sgetrf(2,2,a,2,piv,info)
      call require(info == 1 .and. all(piv == [1,2]), 'sgetrf singular INFO and pivots')
      call sgetrf(-1,2,a,2,piv,info)
      call require(info == -1, 'sgetrf invalid argument INFO')

      a = original
      piv = -99
      call ssytrf('U',2,a,2,piv,query,-1,info)
      call require(info == 0 .and. query(1) > 0 .and. all(piv == -99), 'ssytrf query preserves pivots')
      call ssytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv == [1,2]), 'ssytrf pivots')
      b = original
      call ssytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'ssytrs')
      a = zero
      call ssytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 2 .and. all(piv == [1,2]), 'ssytrf singular INFO and pivots')
      a = zero
      a(1,2) = one
      a(2,1) = one
      b = a
      call ssytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv < 0), 'ssytrf signed block pivots')
      call ssytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'ssytrs block pivots')

      a = original
      call sgeqrf(2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'sgeqrf query')
      call sgeqrf(2,2,a,2,tau,work,1024,info)
      call require(info == 0, 'sgeqrf')
      b = zero
      b(1,1) = one
      b(2,2) = one
      call sormqr('L','N',2,2,2,a,2,tau,b,2,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'sormqr query')
      call sormqr('L','N',2,2,2,a,2,tau,b,2,work,1024,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'sormqr')
      call sorgqr(2,2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'sorgqr query')
      call sorgqr(2,2,2,a,2,tau,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-one) < tol, 'sorgqr')

      a = original
      piv = -99
      call sgeqp3(2,2,a,2,piv,tau,query,-1,info)
      call require(info == 0 .and. query(1) > 0 .and. all(piv == -99), 'sgeqp3 query preserves pivots')
      piv = 0
      call sgeqp3(2,2,a,2,piv,tau,work,1024,info)
      call require(info == 0 .and. all(piv == [2,1]), 'sgeqp3 pivots')
      a = original
      piv = -99
      rank = -99
      call sgeqp3mod(2,2,a,2,piv,tau,query,-1,info,rank,zero,zero)
      call require(info == 0 .and. all(piv == -99) .and. rank == -99, 'sgeqp3mod query preserves outputs')
      piv = 0
      call sgeqp3mod(2,2,a,2,piv,tau,work,1024,info,rank,zero,zero)
      call require(info == 0 .and. all(piv == [2,1]) .and. rank == 2, 'sgeqp3mod rank and pivots')
      a = original
      piv = 0
      call sgeqp3mod(2,2,a,2,piv,tau,work,1024,info,rank,0.8_4,zero)
      call require(info == 0 .and. rank == 1 .and. all(piv == [2,1]), 'sgeqp3mod truncated rank')
      piv = -99
      rank = -99
      call sgeqp3mod(0,2,a,2,piv,tau,work,1024,info,rank,zero,zero)
      call require(info == 0 .and. rank == 0 .and. all(piv == -99), 'sgeqp3mod empty rank')

      a = original
      call sgesvd('A','A',2,2,a,2,singular,u,2,vt,2,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'sgesvd query')
      call sgesvd('A','A',2,2,a,2,singular,u,2,vt,2,work,1024,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'sgesvd')
      a = original
      iwork = -99
      call sgesdd('A',2,2,a,2,singular,u,2,vt,2,query,-1,iwork,info)
      call require(info == 0 .and. all(iwork == -99), 'sgesdd query preserves IWORK')
      call sgesdd('A',2,2,a,2,singular,u,2,vt,2,work,1024,iwork,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'sgesdd')
      a = original
      call sgeev('V','V',2,a,2,wr,wi,u,2,vt,2,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'sgeev query')
      call sgeev('V','V',2,a,2,wr,wi,u,2,vt,2,work,1024,info)
      call require(info == 0 .and. all(abs(wr-[2.0_4,3.0_4]) < tol) .and. all(abs(wi) < tol), 'sgeev')

      a = zero
      a(1,2) = -one
      a(2,1) = one
      selected = [.false.,.true.]
      m = -99
      call strevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,query,-1,info)
      call require(info == 0 .and. m == 2 .and. all(selected .eqv. [.true.,.false.]), 'strevc3mod query SELECT')
      call strevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,work,1024,info)
      call require(info == 0 .and. m == 2, 'strevc3mod logical and count outputs')
      a = original
      call require(abs(slange('I',2,2,a,2,rwork)-3) < tol, 'slange')
      call slacpy('A',2,2,a,2,b,2)
      call require(all(abs(a-b) < tol), 'slacpy')
      call sgemm('N','N',2,2,2,one,a,2,b,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'sgemm')
      call elements_s(a,b,u)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'sgemm/slacpy assumed-shape elements')
      call slacpy('A',2,2,a,2,work,2)
      call sgemm('N','N',2,2,2,one,a,2,work,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'sgemm/slacpy rank-one storage')
      work = -99
      call slacpy('A',2,2,a,2,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(a,[4])) < tol), 'slacpy noncontiguous output')
      call require(all(abs(work(2:8:2)+99) < tol), 'slacpy preserves adjacent storage')
      call sgemm('N','N',2,2,2,one,a,2,b,2,zero,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(u,[4])) < tol), 'sgemm noncontiguous output')
      call strsm('L','U','N','N',2,2,one,a,2,b,2)
      call require(abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'strsm')
      call saxpy(2,one,a(:,1),1,b(:,1),1)
      call require(abs(b(1,1)-3) < tol, 'saxpy')
   end subroutine

   subroutine test_d()
      real(kind=8), parameter :: one = 1.0_8, zero = 0.0_8, tol = 1.0e-10_8
      real(kind=8) :: a(2,2), original(2,2), b(2,2), u(2,2), vt(2,2)
      real(kind=8) :: work(1024), query(1), tau(2), singular(2), wr(2), wi(2), rwork(1024)
      integer :: piv(2), info, rank, iwork(16), m
      logical :: selected(2)

      original = zero
      original(1,1) = 2
      original(2,2) = 3
      a = original
      call dgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'dgetrf pivots')
      b = original
      call dgetrs('N',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'dgetrs')
      call dgetri(2,a,2,piv,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'dgetri query')
      call dgetri(2,a,2,piv,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-0.5_8) < tol, 'dgetri')
      a = original
      call dgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'dgetrfmod pivots')
      a = reshape([zero,2.0_8,one,3.0_8], [2,2])
      call dgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'dgetrf row interchange')
      a = reshape([zero,2.0_8,one,3.0_8], [2,2])
      call dgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'dgetrfmod row interchange')
      a = zero
      call dgetrf(2,2,a,2,piv,info)
      call require(info == 1 .and. all(piv == [1,2]), 'dgetrf singular INFO and pivots')
      call dgetrf(-1,2,a,2,piv,info)
      call require(info == -1, 'dgetrf invalid argument INFO')

      a = original
      piv = -99
      call dsytrf('U',2,a,2,piv,query,-1,info)
      call require(info == 0 .and. query(1) > 0 .and. all(piv == -99), 'dsytrf query preserves pivots')
      call dsytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv == [1,2]), 'dsytrf pivots')
      b = original
      call dsytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'dsytrs')
      a = zero
      call dsytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 2 .and. all(piv == [1,2]), 'dsytrf singular INFO and pivots')
      a = zero
      a(1,2) = one
      a(2,1) = one
      b = a
      call dsytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv < 0), 'dsytrf signed block pivots')
      call dsytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'dsytrs block pivots')

      a = original
      call dgeqrf(2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'dgeqrf query')
      call dgeqrf(2,2,a,2,tau,work,1024,info)
      call require(info == 0, 'dgeqrf')
      b = zero
      b(1,1) = one
      b(2,2) = one
      call dormqr('L','N',2,2,2,a,2,tau,b,2,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'dormqr query')
      call dormqr('L','N',2,2,2,a,2,tau,b,2,work,1024,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'dormqr')
      call dorgqr(2,2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'dorgqr query')
      call dorgqr(2,2,2,a,2,tau,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-one) < tol, 'dorgqr')

      a = original
      piv = -99
      call dgeqp3(2,2,a,2,piv,tau,query,-1,info)
      call require(info == 0 .and. query(1) > 0 .and. all(piv == -99), 'dgeqp3 query preserves pivots')
      piv = 0
      call dgeqp3(2,2,a,2,piv,tau,work,1024,info)
      call require(info == 0 .and. all(piv == [2,1]), 'dgeqp3 pivots')
      a = original
      piv = -99
      rank = -99
      call dgeqp3mod(2,2,a,2,piv,tau,query,-1,info,rank,zero,zero)
      call require(info == 0 .and. all(piv == -99) .and. rank == -99, 'dgeqp3mod query preserves outputs')
      piv = 0
      call dgeqp3mod(2,2,a,2,piv,tau,work,1024,info,rank,zero,zero)
      call require(info == 0 .and. all(piv == [2,1]) .and. rank == 2, 'dgeqp3mod rank and pivots')
      a = original
      piv = 0
      call dgeqp3mod(2,2,a,2,piv,tau,work,1024,info,rank,0.8_8,zero)
      call require(info == 0 .and. rank == 1 .and. all(piv == [2,1]), 'dgeqp3mod truncated rank')
      piv = -99
      rank = -99
      call dgeqp3mod(0,2,a,2,piv,tau,work,1024,info,rank,zero,zero)
      call require(info == 0 .and. rank == 0 .and. all(piv == -99), 'dgeqp3mod empty rank')

      a = original
      call dgesvd('A','A',2,2,a,2,singular,u,2,vt,2,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'dgesvd query')
      call dgesvd('A','A',2,2,a,2,singular,u,2,vt,2,work,1024,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'dgesvd')
      a = original
      iwork = -99
      call dgesdd('A',2,2,a,2,singular,u,2,vt,2,query,-1,iwork,info)
      call require(info == 0 .and. all(iwork == -99), 'dgesdd query preserves IWORK')
      call dgesdd('A',2,2,a,2,singular,u,2,vt,2,work,1024,iwork,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'dgesdd')
      a = original
      call dgeev('V','V',2,a,2,wr,wi,u,2,vt,2,query,-1,info)
      call require(info == 0 .and. query(1) > 0, 'dgeev query')
      call dgeev('V','V',2,a,2,wr,wi,u,2,vt,2,work,1024,info)
      call require(info == 0 .and. all(abs(wr-[2.0_8,3.0_8]) < tol) .and. all(abs(wi) < tol), 'dgeev')

      a = zero
      a(1,2) = -one
      a(2,1) = one
      selected = [.false.,.true.]
      m = -99
      call dtrevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,query,-1,info)
      call require(info == 0 .and. m == 2 .and. all(selected .eqv. [.true.,.false.]), 'dtrevc3mod query SELECT')
      call dtrevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,work,1024,info)
      call require(info == 0 .and. m == 2, 'dtrevc3mod logical and count outputs')
      a = original
      call require(abs(dlange('I',2,2,a,2,rwork)-3) < tol, 'dlange')
      call dlacpy('A',2,2,a,2,b,2)
      call require(all(abs(a-b) < tol), 'dlacpy')
      call dgemm('N','N',2,2,2,one,a,2,b,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'dgemm')
      call elements_d(a,b,u)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'dgemm/dlacpy assumed-shape elements')
      call dlacpy('A',2,2,a,2,work,2)
      call dgemm('N','N',2,2,2,one,a,2,work,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'dgemm/dlacpy rank-one storage')
      work = -99
      call dlacpy('A',2,2,a,2,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(a,[4])) < tol), 'dlacpy noncontiguous output')
      call require(all(abs(work(2:8:2)+99) < tol), 'dlacpy preserves adjacent storage')
      call dgemm('N','N',2,2,2,one,a,2,b,2,zero,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(u,[4])) < tol), 'dgemm noncontiguous output')
      call dtrsm('L','U','N','N',2,2,one,a,2,b,2)
      call require(abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'dtrsm')
      call daxpy(2,one,a(:,1),1,b(:,1),1)
      call require(abs(b(1,1)-3) < tol, 'daxpy')
   end subroutine

   subroutine test_c()
      complex(kind=4), parameter :: one = (1.0_4,0.0_4), zero = (0.0_4,0.0_4)
      real(kind=4), parameter :: tol = 1.0e-4_4, rzero = 0.0_4
      complex(kind=4) :: a(2,2), original(2,2), b(2,2), u(2,2), vt(2,2)
      complex(kind=4) :: work(1024), query(1), tau(2), eigenvalues(2)
      real(kind=4) :: singular(2), rwork(1024), rquery(1)
      integer :: piv(2), info, rank, iwork(16), m
      logical :: selected(2)

      original = zero
      original(1,1) = 2
      original(2,2) = 3
      a = original
      call cgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'cgetrf pivots')
      b = original
      call cgetrs('N',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'cgetrs')
      call cgetri(2,a,2,piv,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'cgetri query')
      call cgetri(2,a,2,piv,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-0.5_4) < tol, 'cgetri')
      a = original
      call cgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'cgetrfmod pivots')
      a = reshape([zero,2*one,one,3*one], [2,2])
      call cgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'cgetrf row interchange')
      a = reshape([zero,2*one,one,3*one], [2,2])
      call cgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'cgetrfmod row interchange')
      a = zero
      call cgetrf(2,2,a,2,piv,info)
      call require(info == 1 .and. all(piv == [1,2]), 'cgetrf singular INFO and pivots')
      call cgetrf(-1,2,a,2,piv,info)
      call require(info == -1, 'cgetrf invalid argument INFO')

      a = original
      piv = -99
      call csytrf('U',2,a,2,piv,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0 .and. all(piv == -99), 'csytrf query preserves pivots')
      call csytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv == [1,2]), 'csytrf pivots')
      b = original
      call csytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'csytrs')
      a = zero
      call csytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 2 .and. all(piv == [1,2]), 'csytrf singular INFO and pivots')
      a = zero
      a(1,2) = one
      a(2,1) = one
      b = a
      call csytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv < 0), 'csytrf signed block pivots')
      call csytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'csytrs block pivots')

      a = original
      call cgeqrf(2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'cgeqrf query')
      call cgeqrf(2,2,a,2,tau,work,1024,info)
      call require(info == 0, 'cgeqrf')
      b = zero
      b(1,1) = one
      b(2,2) = one
      call cunmqr('L','N',2,2,2,a,2,tau,b,2,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'cunmqr query')
      call cunmqr('L','N',2,2,2,a,2,tau,b,2,work,1024,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'cunmqr')
      call cungqr(2,2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'cungqr query')
      call cungqr(2,2,2,a,2,tau,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-one) < tol, 'cungqr')

      a = original
      piv = -99
      call cgeqp3(2,2,a,2,piv,tau,query,-1,rwork,info)
      call require(info == 0 .and. real(query(1)) > 0 .and. all(piv == -99), 'cgeqp3 query preserves pivots')
      piv = 0
      call cgeqp3(2,2,a,2,piv,tau,work,1024,rwork,info)
      call require(info == 0 .and. all(piv == [2,1]), 'cgeqp3 pivots')
      a = original
      piv = -99
      rank = -99
      call cgeqp3mod(2,2,a,2,piv,tau,query,-1,rwork,info,rank,rzero,rzero)
      call require(info == 0 .and. all(piv == -99) .and. rank == -99, 'cgeqp3mod query preserves outputs')
      piv = 0
      call cgeqp3mod(2,2,a,2,piv,tau,work,1024,rwork,info,rank,rzero,rzero)
      call require(info == 0 .and. all(piv == [2,1]) .and. rank == 2, 'cgeqp3mod rank and pivots')
      a = original
      piv = 0
      call cgeqp3mod(2,2,a,2,piv,tau,work,1024,rwork,info,rank,0.8_4,rzero)
      call require(info == 0 .and. rank == 1 .and. all(piv == [2,1]), 'cgeqp3mod truncated rank')
      piv = -99
      rank = -99
      call cgeqp3mod(0,2,a,2,piv,tau,work,1024,rwork,info,rank,rzero,rzero)
      call require(info == 0 .and. rank == 0 .and. all(piv == -99), 'cgeqp3mod empty rank')

      a = original
      call cgesvd('A','A',2,2,a,2,singular,u,2,vt,2,query,-1,rwork,info)
      call require(info == 0 .and. real(query(1)) > 0, 'cgesvd query')
      call cgesvd('A','A',2,2,a,2,singular,u,2,vt,2,work,1024,rwork,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'cgesvd')
      a = original
      iwork = -99
      call cgesdd('A',2,2,a,2,singular,u,2,vt,2,query,-1,rwork,iwork,info)
      call require(info == 0 .and. all(iwork == -99), 'cgesdd query preserves IWORK')
      call cgesdd('A',2,2,a,2,singular,u,2,vt,2,work,1024,rwork,iwork,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'cgesdd')
      a = original
      call cgeev('V','V',2,a,2,eigenvalues,u,2,vt,2,query,-1,rwork,info)
      call require(info == 0 .and. real(query(1)) > 0, 'cgeev query')
      call cgeev('V','V',2,a,2,eigenvalues,u,2,vt,2,work,1024,rwork,info)
      call require(info == 0 .and. all(abs(eigenvalues-[2.0_4,3.0_4]) < tol), 'cgeev')

      a = original
      selected = [.false.,.true.]
      m = -99
      call ctrevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,query,-1,rquery,-1,info)
      call require(info == 0 .and. m == 1 .and. all(selected .eqv. [.false.,.true.]), 'ctrevc3mod query SELECT')
      call require(real(query(1)) > 0 .and. rquery(1) > 0, 'ctrevc3mod query workspaces')
      call ctrevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,work,1024,rwork,1024,info)
      call require(info == 0 .and. m == 1, 'ctrevc3mod logical and count outputs')
      call require(abs(clange('I',2,2,a,2,rwork)-3) < tol, 'clange')
      call clacpy('A',2,2,a,2,b,2)
      call require(all(abs(a-b) < tol), 'clacpy')
      call cgemm('N','N',2,2,2,one,a,2,b,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'cgemm')
      call elements_c(a,b,u)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'cgemm/clacpy assumed-shape elements')
      call clacpy('A',2,2,a,2,work,2)
      call cgemm('N','N',2,2,2,one,a,2,work,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'cgemm/clacpy rank-one storage')
      work = -99
      call clacpy('A',2,2,a,2,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(a,[4])) < tol), 'clacpy noncontiguous output')
      call require(all(abs(work(2:8:2)+99) < tol), 'clacpy preserves adjacent storage')
      call cgemm('N','N',2,2,2,one,a,2,b,2,zero,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(u,[4])) < tol), 'cgemm noncontiguous output')
      call ctrsm('L','U','N','N',2,2,one,a,2,b,2)
      call require(abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'ctrsm')
      call caxpy(2,one,a(:,1),1,b(:,1),1)
      call require(abs(b(1,1)-3) < tol, 'caxpy')
   end subroutine

   subroutine test_z()
      complex(kind=8), parameter :: one = (1.0_8,0.0_8), zero = (0.0_8,0.0_8)
      real(kind=8), parameter :: tol = 1.0e-10_8, rzero = 0.0_8
      complex(kind=8) :: a(2,2), original(2,2), b(2,2), u(2,2), vt(2,2)
      complex(kind=8) :: work(1024), query(1), tau(2), eigenvalues(2)
      real(kind=8) :: singular(2), rwork(1024), rquery(1)
      integer :: piv(2), info, rank, iwork(16), m
      logical :: selected(2)

      original = zero
      original(1,1) = 2
      original(2,2) = 3
      a = original
      call zgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'zgetrf pivots')
      b = original
      call zgetrs('N',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'zgetrs')
      call zgetri(2,a,2,piv,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'zgetri query')
      call zgetri(2,a,2,piv,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-0.5_8) < tol, 'zgetri')
      a = original
      call zgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [1,2]), 'zgetrfmod pivots')
      a = reshape([zero,2*one,one,3*one], [2,2])
      call zgetrf(2,2,a,2,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'zgetrf row interchange')
      a = reshape([zero,2*one,one,3*one], [2,2])
      call zgetrfmod(2,2,a,2,tol,piv,info)
      call require(info == 0 .and. all(piv == [2,2]), 'zgetrfmod row interchange')
      a = zero
      call zgetrf(2,2,a,2,piv,info)
      call require(info == 1 .and. all(piv == [1,2]), 'zgetrf singular INFO and pivots')
      call zgetrf(-1,2,a,2,piv,info)
      call require(info == -1, 'zgetrf invalid argument INFO')

      a = original
      piv = -99
      call zsytrf('U',2,a,2,piv,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0 .and. all(piv == -99), 'zsytrf query preserves pivots')
      call zsytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv == [1,2]), 'zsytrf pivots')
      b = original
      call zsytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'zsytrs')
      a = zero
      call zsytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 2 .and. all(piv == [1,2]), 'zsytrf singular INFO and pivots')
      a = zero
      a(1,2) = one
      a(2,1) = one
      b = a
      call zsytrf('U',2,a,2,piv,work,1024,info)
      call require(info == 0 .and. all(piv < 0), 'zsytrf signed block pivots')
      call zsytrs('U',2,2,a,2,piv,b,2,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'zsytrs block pivots')

      a = original
      call zgeqrf(2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'zgeqrf query')
      call zgeqrf(2,2,a,2,tau,work,1024,info)
      call require(info == 0, 'zgeqrf')
      b = zero
      b(1,1) = one
      b(2,2) = one
      call zunmqr('L','N',2,2,2,a,2,tau,b,2,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'zunmqr query')
      call zunmqr('L','N',2,2,2,a,2,tau,b,2,work,1024,info)
      call require(info == 0 .and. abs(b(1,1)-one) < tol, 'zunmqr')
      call zungqr(2,2,2,a,2,tau,query,-1,info)
      call require(info == 0 .and. real(query(1)) > 0, 'zungqr query')
      call zungqr(2,2,2,a,2,tau,work,1024,info)
      call require(info == 0 .and. abs(a(1,1)-one) < tol, 'zungqr')

      a = original
      piv = -99
      call zgeqp3(2,2,a,2,piv,tau,query,-1,rwork,info)
      call require(info == 0 .and. real(query(1)) > 0 .and. all(piv == -99), 'zgeqp3 query preserves pivots')
      piv = 0
      call zgeqp3(2,2,a,2,piv,tau,work,1024,rwork,info)
      call require(info == 0 .and. all(piv == [2,1]), 'zgeqp3 pivots')
      a = original
      piv = -99
      rank = -99
      call zgeqp3mod(2,2,a,2,piv,tau,query,-1,rwork,info,rank,rzero,rzero)
      call require(info == 0 .and. all(piv == -99) .and. rank == -99, 'zgeqp3mod query preserves outputs')
      piv = 0
      call zgeqp3mod(2,2,a,2,piv,tau,work,1024,rwork,info,rank,rzero,rzero)
      call require(info == 0 .and. all(piv == [2,1]) .and. rank == 2, 'zgeqp3mod rank and pivots')
      a = original
      piv = 0
      call zgeqp3mod(2,2,a,2,piv,tau,work,1024,rwork,info,rank,0.8_8,rzero)
      call require(info == 0 .and. rank == 1 .and. all(piv == [2,1]), 'zgeqp3mod truncated rank')
      piv = -99
      rank = -99
      call zgeqp3mod(0,2,a,2,piv,tau,work,1024,rwork,info,rank,rzero,rzero)
      call require(info == 0 .and. rank == 0 .and. all(piv == -99), 'zgeqp3mod empty rank')

      a = original
      call zgesvd('A','A',2,2,a,2,singular,u,2,vt,2,query,-1,rwork,info)
      call require(info == 0 .and. real(query(1)) > 0, 'zgesvd query')
      call zgesvd('A','A',2,2,a,2,singular,u,2,vt,2,work,1024,rwork,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'zgesvd')
      a = original
      iwork = -99
      call zgesdd('A',2,2,a,2,singular,u,2,vt,2,query,-1,rwork,iwork,info)
      call require(info == 0 .and. all(iwork == -99), 'zgesdd query preserves IWORK')
      call zgesdd('A',2,2,a,2,singular,u,2,vt,2,work,1024,rwork,iwork,info)
      call require(info == 0 .and. abs(singular(1)-3) < tol, 'zgesdd')
      a = original
      call zgeev('V','V',2,a,2,eigenvalues,u,2,vt,2,query,-1,rwork,info)
      call require(info == 0 .and. real(query(1)) > 0, 'zgeev query')
      call zgeev('V','V',2,a,2,eigenvalues,u,2,vt,2,work,1024,rwork,info)
      call require(info == 0 .and. all(abs(eigenvalues-[2.0_8,3.0_8]) < tol), 'zgeev')

      a = original
      selected = [.false.,.true.]
      m = -99
      call ztrevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,query,-1,rquery,-1,info)
      call require(info == 0 .and. m == 1 .and. all(selected .eqv. [.false.,.true.]), 'ztrevc3mod query SELECT')
      call require(real(query(1)) > 0 .and. rquery(1) > 0, 'ztrevc3mod query workspaces')
      call ztrevc3mod('R','S',selected,2,a,2,u,2,vt,2,2,m,work,1024,rwork,1024,info)
      call require(info == 0 .and. m == 1, 'ztrevc3mod logical and count outputs')
      call require(abs(zlange('I',2,2,a,2,rwork)-3) < tol, 'zlange')
      call zlacpy('A',2,2,a,2,b,2)
      call require(all(abs(a-b) < tol), 'zlacpy')
      call zgemm('N','N',2,2,2,one,a,2,b,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'zgemm')
      call elements_z(a,b,u)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'zgemm/zlacpy assumed-shape elements')
      call zlacpy('A',2,2,a,2,work,2)
      call zgemm('N','N',2,2,2,one,a,2,work,2,zero,u,2)
      call require(abs(u(1,1)-4) < tol .and. abs(u(2,2)-9) < tol, 'zgemm/zlacpy rank-one storage')
      work = -99
      call zlacpy('A',2,2,a,2,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(a,[4])) < tol), 'zlacpy noncontiguous output')
      call require(all(abs(work(2:8:2)+99) < tol), 'zlacpy preserves adjacent storage')
      call zgemm('N','N',2,2,2,one,a,2,b,2,zero,work(1:8:2),2)
      call require(all(abs(work(1:8:2)-reshape(u,[4])) < tol), 'zgemm noncontiguous output')
      call ztrsm('L','U','N','N',2,2,one,a,2,b,2)
      call require(abs(b(1,1)-one) < tol .and. abs(b(2,2)-one) < tol, 'ztrsm')
      call zaxpy(2,one,a(:,1),1,b(:,1),1)
      call require(abs(b(1,1)-3) < tol, 'zaxpy')
   end subroutine

   subroutine elements_s(a,b,c)
      real(kind=4) :: a(:,:), b(:,:), c(:,:)
      call slacpy('A',2,2,a(1,1),2,b(1,1),2)
      call sgemm('N','N',2,2,2,1.0_4,a(1,1),2,b(1,1),2,0.0_4,c(1,1),2)
   end subroutine

   subroutine elements_d(a,b,c)
      real(kind=8) :: a(:,:), b(:,:), c(:,:)
      call dlacpy('A',2,2,a(1,1),2,b(1,1),2)
      call dgemm('N','N',2,2,2,1.0_8,a(1,1),2,b(1,1),2,0.0_8,c(1,1),2)
   end subroutine

   subroutine elements_c(a,b,c)
      complex(kind=4) :: a(:,:), b(:,:), c(:,:)
      call clacpy('A',2,2,a(1,1),2,b(1,1),2)
      call cgemm('N','N',2,2,2,(1.0_4,0.0_4),a(1,1),2,b(1,1),2,(0.0_4,0.0_4),c(1,1),2)
   end subroutine

   subroutine elements_z(a,b,c)
      complex(kind=8) :: a(:,:), b(:,:), c(:,:)
      call zlacpy('A',2,2,a(1,1),2,b(1,1),2)
      call zgemm('N','N',2,2,2,(1.0_8,0.0_8),a(1,1),2,b(1,1),2,(0.0_8,0.0_8),c(1,1),2)
   end subroutine

end program
