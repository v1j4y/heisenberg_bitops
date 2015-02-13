BEGIN_PROVIDER [integer,addt,(nt1,2)]
&BEGIN_PROVIDER [integer,adda,(nt1*nt2,2)]

    implicit none
    BEGIN_DOC
    ! generates det and addresses
    END_DOC
    
    integer(4)::i
    integer::j,tmp,tmp2

    countbit=0
    i=2**(ntrou)-2
    do while(.TRUE.)
!   print *,bit_size(i)-leadz(i)
    if((bit_size(i)-leadz(i)).le.nsites)then
    if(popcnt(i).eq.(ntrou))then
        countbit+=1
        addt(countbit,1)=countbit
        addt(countbit,2)=i
        write(6,15)addt(countbit,2),addt(countbit,1),addt(countbit,2),bit_size(i)-leadz(i)
    endif
    else
    EXIT
    endif
    i+=1
    enddo
    print *,countbit

!C  doing alpha electrons
    countbit=0
    do j=1,nt1
!   i=2**((nsites-ntrou)-nbeta)-2
    i=0
    do while(.TRUE.)
!   print *,bit_size(i)-leadz(i)
    if((bit_size(i)-leadz(i)).le.(nsites))then
    if(popcnt(i).eq.(nalpha))then
        tmp=XOR(addt(j,2),i)
        if(popcnt(tmp).eq.(nalpha+ntrou))then
            countbit+=1
            adda(countbit,1)=countbit
            adda(countbit,2)=i
            write(6,14)adda(countbit,2),adda(countbit,1)
        endif
    endif
    else
    EXIT
    endif
    i+=1
    enddo
    enddo
    print *,countbit



10  FORMAT(B64,I8,F8.2)
15  FORMAT(B64,I8,I8,I8)
11  FORMAT(B64,I3,B64)
12  FORMAT(I5,$)
13  FORMAT(B64,B64)
18  FORMAT(B64,B64,B64)
17  FORMAT(B64,B64,I8)
14  FORMAT(B64,I8)
16  FORMAT(B64,I8,I8)
END_PROVIDER
