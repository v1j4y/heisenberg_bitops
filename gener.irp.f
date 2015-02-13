BEGIN_PROVIDER [integer,addt,(maxdet,2)]
&BEGIN_PROVIDER [integer,adda,(maxdet,2)]
&BEGIN_PROVIDER [integer,addb,(maxdet,2)]

    implicit none
    BEGIN_DOC
    ! generates det and addresses
    END_DOC
    
    integer(4)::i
    integer::j,tmp,tmp2

    countbit=0
    i=2**(nsites-ntrou)-2
    do while(.TRUE.)
!   print *,bit_size(i)-leadz(i)
    if((bit_size(i)-leadz(i)).le.nsites)then
    if(popcnt(i).eq.(nsites-ntrou))then
        countbit+=1
        addt(countbit,1)=countbit
        addt(countbit,2)=i
!       write(6,15)add(countbit,2),add(countbit,1),add(countbit,2),bit_size(i)-leadz(i)
    endif
    else
    EXIT
    endif
    i+=1
    enddo
    print *,countbit

!C  doing the rest 
    countbit=0
    i=2**((nsites-ntrou)-nbeta)-2
    print *,'i=',i
    do while(.TRUE.)
!   print *,bit_size(i)-leadz(i)
    if((bit_size(i)-leadz(i)).le.(nsites-ntrou))then
    if(popcnt(i).eq.((nsites-ntrou)-nbeta))then
        countbit+=1
        add(countbit,3)=countbit
        add(countbit,4)=i
        write(6,15)add(countbit,4),add(countbit,3),add(countbit,4),bit_size(i)-leadz(i)
    endif
    else
    EXIT
    endif
    i+=1
    enddo
    print *,countbit



10  FORMAT(B64,I8,F8.2)
15  FORMAT(B64,I8,I8,I8)
11  FORMAT(B64,I3,B64)
12  FORMAT(I5,$)
13  FORMAT(B64,B64)
14  FORMAT(B64,I8)
16  FORMAT(B64,I8,I8)
END_PROVIDER
