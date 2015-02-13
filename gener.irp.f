BEGIN_PROVIDER [integer,add,(maxdet,maxdet)]

    implicit none
    BEGIN_DOC
    ! generates det and addresses
    END_DOC
    
    integer(4)::i
    integer::j,tmp,tmp2

    countbit=0
    i=2**(nsites-1)-2
    do while(.TRUE.)
!   print *,bit_size(i)-leadz(i)
    if((bit_size(i)-leadz(i)).le.nsites)then
    if(popcnt(i).eq.(nsites-ntrou))then
        countbit+=1
        add(countbit,1)=countbit
        add(countbit,2)=i
!       write(6,15)add(countbit,2),add(countbit,1),add(countbit,2),bit_size(i)-leadz(i)
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
