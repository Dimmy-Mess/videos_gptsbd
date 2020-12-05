MODULE MATRIX
CONTAINS
SUBROUTINE H_CHAIN(H,N,M)
	
	INTEGER, INTENT(IN):: N,M
	INTEGER, DIMENSION(N,N), INTENT(INOUT):: H		

	INTEGER:: i, i_plus
	LOGICAL::  DOWN, END_NET

	H = 0
	
	DO i=1, N
		i_plus = i+1
		i_M = i+M
		
		DOWN = (MOD(i,M) .EQ. 0) !Testa se é um spin da extremidade inferior
		END_NET = (i_M .LE. N) 	 !Testa se o próximo spin faz parte da rede

		
		IF (DOWN .EQV. .TRUE.) THEN
			IF (END_NET .EQV. .TRUE.) H(i,i_M) = 1
			
		ELSE
			H(i,i_plus) = 1
			IF (END_NET .EQV. .TRUE.) H(i,i_M) = 1
		
		END IF
	END DO
	
	H = H+TRANSPOSE(H)
END SUBROUTINE H_CHAIN


SUBROUTINE SHOW(H)
	
	INTEGER,INTENT(IN):: H(:,:)
	CHARACTER(LEN=1), DIMENSION(:,:),ALLOCATABLE:: GRID
	
	INTEGER:: i,j,N(2)
	
	N = SHAPE(H)
	ALLOCATE(GRID(N(1),N(2)))
	
	GRID = 'X'
	
	DO i = 1, N(1)
    	DO j=1,N(2)
        	WRITE(GRID(i,j), "(i1)") H(i,j)
   	 	END DO
	END DO
	
	DO I=1,N(1)
    	PRINT*,'i:',i, '- ',Grid(i,:)
	END DO

END SUBROUTINE SHOW

END MODULE MATRIX

!###############################################################################
PROGRAM mod_ab
USE MATRIX

INTEGER :: M,N
INTEGER, DIMENSION(:,:), ALLOCATABLE:: H, H2


print*,'Quantos elementos na rede?'
read*,N
print*, 'Quantos elementos formam uma coluna?'
read*,M

ALLOCATE(H(N,N))


CALL H_CHAIN(H,N,M)
PRINT*,'RESULTADO (H_chain)'
CALL SHOW(H)

END PROGRAM mod_ab


