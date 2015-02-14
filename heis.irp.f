program heis
    implicit none
    BEGIN_DOC
    ! main prog
    ! prints eigenvalues and eigenvectors
    END_DOC
    integer::i,j,k
    double precision, allocatable :: EVAL(:),EVEC(:,:),tmp(:)
    double precision, allocatable :: W(:),WORK(:),AP(:)
    character*1 JOBZ,UPLO
    integer LDA,LWORK,INFO,N
    N=NDET
    allocate (AP(N*N))
    allocate (W(N))
    allocate (tmp(N))
    allocate (WORK(N*(2*N)))
    LDA=N
    UPLO='U'
    JOBZ='V'


    K=0
    do j=1,N
    do i=1,j
      AP(i+(j-1)*j/2)=A(i,j)
      K=K+1
    enddo
    enddo

!   LWORK=-1
!   call DSPEV( JOBZ, UPLO, N, AP, W, A, LDA, WORK, INFO)
!   LWORK=INT(WORK(1))
!   write(6,*)LWORK

    call DSPEV( JOBZ, UPLO, N, AP, W, A, LDA, WORK, INFO)
!   write(6,*)INFO

    if (INFO.ne.0)then
        print*,'Error at dspev'
        call exit(1)
    endif

    do i=1,N
      write(12,1022)i,W(i)
    enddo

    do i=1,N
      write(22,1022)i,(A(i,j),j=1,N)
    enddo


11   FORMAT((F8.2,' '),$)
1022 FORMAT(3x,I3,10(2x,F12.4))
end
