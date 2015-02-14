BEGIN_PROVIDER [real(8),A,(ndet,ndet)]

    implicit none
    BEGIN_DOC
    ! provides the hamiltonian matrix
    END_DOC
    integer::i,j,k,l,deti,detj
    logical::yalt
    integer::sgn,tmp
    

    do i=1,nt1
    do j=1,nt2
        do k=1,nt1
        do l=1,nt2
            if(yalt(IOR(addt(i,2),adda(nt2*(i-1)+j,2)),IOR(addt(k,2),adda(nt2*(k-1)+l,2))))then
                if(yalt(addt(i,2),addt(k,2)).or. addt(i,2).eq.addt(k,2))then
                A(nt2*(i-1)+j,nt2*(k-1)+l)=1d0
                A(nt2*(k-1)+l,nt2*(i-1)+j)=A(nt2*(i-1)+j,nt2*(k-1)+l)
                endif
            elseif(IOR(addt(i,2),adda(nt2*(i-1)+j,2))                                               &
            .eq. IOR(addt(k,2),adda(nt2*(k-1)+l,2)) .and. (nt2*(k-1)+l).ne.(nt2*(i-1)+j))then
                if(yalt(addt(i,2),addt(k,2)).or. addt(i,2).eq.addt(k,2))then
                A(nt2*(i-1)+j,nt2*(k-1)+l)=1d0
                A(nt2*(k-1)+l,nt2*(i-1)+j)=A(nt2*(i-1)+j,nt2*(k-1)+l)
                else
                A(nt2*(i-1)+j,nt2*(k-1)+l)=0d0
                A(nt2*(k-1)+l,nt2*(i-1)+j)=A(nt2*(i-1)+j,nt2*(k-1)+l)
                endif
            else
                A(nt2*(i-1)+j,nt2*(k-1)+l)=0d0
                A(nt2*(k-1)+l,nt2*(i-1)+j)=A(nt2*(i-1)+j,nt2*(k-1)+l)
            endif
!           write(6,13)IOR(addt(i,2),adda(nt2*(i-1)+j,2)),IOR(addt(k,2),adda(nt2*(k-1)+l,2))
        enddo
        enddo
    enddo
    enddo

    do i=1,countbit
    do j=1,countbit
        write(6,11)A(i,j)
    enddo
        write(6,*)
    enddo

11   FORMAT((F8.2,' '),$)
13  FORMAT(B64,B64)
END_PROVIDER

function yalt(deti,detj)
    implicit none
    logical::yalt
    integer,INTENT(IN)::deti
    integer,INTENT(IN)::detj
    integer::i,j,tmp,posr,posl

        tmp=XOR(deti,detj)
        yalt=.FALSE.
        if(popcnt(tmp).eq.2)then
            posr=bit_size(tmp)-leadz(tmp)
            posl=trailz(tmp)+1
            if((posr-posl).eq.1)then
                yalt=.TRUE.
            endif
        endif
14  FORMAT(B64,I8)
end function

function sgn(deti,detj)
    implicit none
    integer::sgn
    integer,INTENT(IN)::deti
    integer,INTENT(IN)::detj

    sgn=1
end function
