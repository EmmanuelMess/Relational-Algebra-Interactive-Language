x = Relation - Relation
print x

print (project(A)(Relation))
print (project(Z)(Relation))


print Relation
X = project(A, B, C)(Relation)
print X
S = select(B = 3.0)(Relation)

print (Relation & Name)

print (Relation & Relation)

print S
A = S + X

print select(E = 7)(Relation)
print select(Relation.E = 7)(Relation)
print select(Relation.E = 5 || Relation.D = 3)(Relation)
print select(Relation.E = 5 && Relation.D = 3)(Relation)
print select(A = "Nombre")(Relation)
print select(C = 2.0)(Relation)
print select(C = N)(Relation)

X = Relation * RelationC
Y = X - Relation
Y = X - X
Y = Y + X
Y = Y - (select(AA="No")(Y))
print Y

Y = Relation * RelationC
Z = project(A, AA)(Y)

print project(A)(Semiempty)

print select(F=-1.10316)(Long)
print select(AA<>"No")(RelationC)
print select(AA<"No")(RelationC)
print select(AA>"No")(RelationC)
print select(F>-1.10316)(Long)
print select(F<-1.10316)(Long)
print select(AA<="No")(RelationC)
print select(AA>="No")(RelationC)
print select(AA>="No" && (AA="No"))(RelationC)
print select(AA>="No" && (AA<>"No"))(RelationC)
print select(AA<"No" || (AA<>"No"))(RelationC)
print select(AA>"No" || (AA<>"No"))(RelationC)
print select(AA>"No" || (AA="No"))(RelationC)

print select(AA=BB)(RelationC)
print select(AA<>BB)(RelationC)
print select(AA<BB)(RelationC)
print select(AA>BB)(RelationC)
print select(AA>BBf)(RelationC)

print select(AA>=BB && (AA=BB))(RelationC)
print select(AA>=BB && (AA<>BB))(RelationC)
print select(AA<BB || (AA<>BB))(RelationC)
print select(AA>BB || (AA<>BB))(RelationC)
print select(AA>BB || (AA=BB))(RelationC)

print rename(X)(select(AA<BB || (AA<>BB))(RelationC))

print (rename(X)(select(AA<BB || (AA<>BB))(RelationC))) & (RelationC)
print (rename(X)(select(AA<BB || (AA<>BB))(RelationC))) & (select(AA<>"No")(RelationC)) & (select(AA<>"Not")(RelationC))

print (Relation|*|RelationDiv)
print (Relation|*|Long)
print (Relation|*|RelationC)

print RelationC|AA=BB|RelationC
print RelationC|AA<>BB|RelationC
print RelationC|RelationC.AA<>afsdf.BB|RelationC
print RelationC|RelationC.AA<>BB|(select(AA=BB)(RelationC))
print Relation|A=B|RelationB
print Relation|Relation.A=RelationB.A|RelationB

print Relation/RelationDiv
print Relation/Relation
print Long/Relation