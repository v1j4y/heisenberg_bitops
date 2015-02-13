BEGIN_PROVIDER [integer,n]
&BEGIN_PROVIDER [integer,maxdet]
&BEGIN_PROVIDER [integer,nsites]
&BEGIN_PROVIDER [integer,ntrou ]
&BEGIN_PROVIDER [integer,countbit]

    implicit none
    BEGIN_DOC
    ! provides rank
    END_DOC
    
    nsites=30
    n=nsites
    ntrou=1
    maxdet=1000
    countbit=0
END_PROVIDER
