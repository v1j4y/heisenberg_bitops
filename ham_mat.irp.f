BEGIN_PROVIDER [real(8),A,(n,n)]

    implicit none
    BEGIN_DOC
    ! provides the hamiltonian matrix
    END_DOC
    integer::i,j,deti,detj
    logical::yalt
    integer::sgn

    do i=1,countbit
        deti=add(i,2)
        do j=1,i
            detj=add(j,2)
            if(yalt(deti,detj))then
                A(i,j)=sgn(deti,detj)*1
                A(j,i)=sgn(deti,detj)*1
            endif
        enddo
    enddo


END_PROVIDER

function yalt(deti,detj)
    implicit none
    logical::yalt
    integer,INTENT(IN)::deti
    integer,INTENT(IN)::detj
    integer::i,j,tmp,posr,posl

        tmp=XOR(deti,detj)
        if(popcnt(tmp).eq.2)then
            posr=bit_size(tmp)-leadz(tmp)
            posl=trailz(tmp)+1
            if((posr-posl).eq.1)then
                yalt=.TRUE.
            endif
        endif
end function

function sgn(deti,detj)
    implicit none
    integer::sgn
    integer,INTENT(IN)::deti
    integer,INTENT(IN)::detj

    sgn=+1
end function
