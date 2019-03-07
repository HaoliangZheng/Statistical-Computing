## Defining Two Matrices for the Following Verification
n=5
A=matrix(1:(n*n),n)
B=matrix(rep(1:(n+1),length.out=n*n),n)

## Properties of the Determinant
#1 |A|=|A'|
all.equal(det(B),det(t(B)))

#2 Multiplying one column of a determinant by k equals
#  the value of multiplying this determinant by k
C=B;C[,1]=C[,1]*3
all.equal(det(C),det(B)*3)

#3 if one column of a matrix A is written as a sum v + w of 
#  two column vectors, then the determinant of A is the sum 
#  of the determinants of the matrices obtained from A by 
#  replacing the column by v and then by w
C=B;C[,1]=n:1
D=B;D[,1]=D[,1]+(n:1)
all.equal(det(D),det(B)+det(C))

#4 A matrix with two identical rows has a determinant of zero
C=B;C[,2]=C[,1]
all.equal(det(C),0)

#5 A matrix with two proportional rows has a determinant of zero
C=B;C[,2]=C[,1]*3
all.equal(det(C),0)

#6 Adding a scalar multiple of one column to another column does 
#  not change the value of the determinant
C=B;C[,2]=C[,2]+C[,1]*3
all.equal(det(B),det(C))

#7 Interchanging any pair of columns of a matrix multiplies 
#  its determinant by -1
C=B;t=C[,1];C[,1]=C[,2];C[,2]=t
all.equal(det(B),det(C)*-1)

#8 one property of the determinant of a specific block matrix
C=cbind(A,0,0,0,0,0)
D=cbind(0,0,0,0,0,B)
E=rbind(C,D)
all.equal(det(E),det(A)*det(B))


## Properties of the Matrix Operations
#9 matrix multiplication is not commutative
A%*%B
B%*%A
all.equal(A%*%B,B%*%A)

#10 (AB)'=A'+B'
C=t(A%*%B);C
D=t(B)%*%t(A);D
all.equal(C,D)

#11 (A+B)'=A'+B'
C=t(A+B);C
D=t(A)+t(B);D
all.equal(C,D)

#12 (kA)'=kA'
C=t(3*A);C
D=t(A)*3;D
all.equal(C,D)

#13 |AB|=|A||B|
all.equal(det(A%*%B),det(A)*det(B))

#14 one property of a specific block matrix
C=cbind(t(A),0,0,0,0,0)
D=cbind(0,0,0,0,0,t(B))
F=rbind(C,D)
t(E);F
all.equal(t(E),F)

## Properties of the Rank
#15 R(A)=R(A')
all.equal(qr(A)$rank,qr(t(A))$rank)

#16 R(A+B)<=R(A)+R(B)
qr(A+B)$rank <= qr(A)$rank + qr(B)$rank

#17 R(AB)<=min[R(A),R(B)]
qr(A%*%B)$rank <= min(qr(A)$rank,qr(B)$rank)

#18 a matrix whose rank is less than n has a determinant of zero
qr(A)$rank < n ; all.equal(det(A),0)
qr(B)$rank == n ; all.equal(det(B),0)

## Properties of the Eigenvalue
#19 The sum of eigenvalues equals the sum of diagonal entries
all.equal(as.numeric(sum(eigen(B)$val)),sum(diag(B)))

#20 The product of eigenvalues equals the determinant
all.equal(as.numeric(prod(eigen(B)$val)),det(B))