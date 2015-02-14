BEGIN_PROVIDER [real(8),A,(nt1*nt2,nt1*nt2)]

    implicit none
    BEGIN_DOC
    ! provides the hamiltonian matrix
    END_DOC
    integer::i,j,deti,detj
    logical::yalt
    integer::sgn,tmp
    
    do i=1,countbit
        deti=addt(i,2)
        do j=1,i
            A(i,j)=0d0
            A(j,i)=0d0
            detj=addt(j,2)
            if(yalt(deti,detj).and. i.ne.j)then
                A(i,j)=sgn(deti,detj)*1
                A(j,i)=A(i,j)
            endif
        enddo
    enddo

    do i=1,countbit
    do j=1,countbit
        write(6,11)A(i,j)
    enddo
        write(6,*)
    enddo

11   FORMAT((F8.2,' '),$)
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
